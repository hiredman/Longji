(ns leiningen.longji
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]]
        [clojure.java.io :only [file]]))

(defn unathorized-use [ns]
  (let [target-ns (create-ns ns)
        {:keys [only-from]} (meta target-ns)
        permited-users (fn [a-ns-name]
                         (some
                          (fn [selecter]
                            (cond
                             (symbol? selecter) (= a-ns-name selecter)
                             (instance? java.util.regex.Pattern
                                        selecter) (re-find selecter
                                                           (str a-ns-name))))
                          only-from))]
    (when only-from
      (doseq [namespace (all-ns)
              :when (some (fn [[a-name a-var]]
                            (and (= ns (.getName (.ns a-var)))
                                 (not (permited-users (.getName namespace)))))
                          (ns-refers namespace))]
        (println (format "Warning: %s used from %s"
                         ns (ns-name namespace)))))))

(defn longji [project & args]
  (let [all-namespaces (->> (file (:source-path project))
                            (find-namespaces-in-dir)
                            (sort))]
    ((resolve 'leiningen.compile/eval-in-project)
     project
     (concat
      ['do
       `(require (quote leiningen.longji))]
      (for [namespace all-namespaces]
        `(try
           (require (quote ~namespace))
           (catch Exception _#)))
      (for [namespace (if (seq args) (map symbol args) all-namespaces)]
        `((resolve (quote leiningen.longji/unathorized-use))
          (quote ~namespace)))))))
