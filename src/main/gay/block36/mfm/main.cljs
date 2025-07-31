(ns gay.block36.mfm.main
  (:require
   [clojure.string :refer [ends-with? replace split]]
   [clojure.core.match :refer [match]]
   ["node:fs" :as fs]
   ["mfm-js" :as mfm]
   [hickory.render :refer [hiccup-to-html]]
   [clojure.walk :as walk]))

(defn read-args []
  (let [args (js->clj js/process.argv)]
    (cond 
      (<= (count args) 2) 
        [:error "No arguments supplied"]
      (<= (count args) 3)
        [:run (args 2)
              (str (if (ends-with? (args 2) ".mfm")
                       (subs (args 2)
                             0
                             (- (count (args 2)) 4))
                       (args 2))
                   ".html")]
      :else [:run (args 2) (args 3)])))

(declare parse-mfm)

(defn sep-words [node]
  (match node
         {:type "text" :props {:text text}}
           (interpose :sep
             (map (fn [word]
                      (list (assoc node
                             :props {:text word})))
                  (split text #" ")))
         {:children children}
                  (map (fn [child]
                           (if (or (and (= (:type node)          "fn")
                                        (= (:name (:props node)) "ruby"))
                                   (= child :sep))
                               child
                               (assoc node
                                      :children
                                      (list child))))
                       (apply concat (map sep-words children)))
         {:type (:or "url"
                     "link")}
           (list node)
         :else (do (println "unsupported node")
                   (list node))))

(defn first-word [words]
  (take-while #(not= :sep %) words))

(defn rest-words [words]
  (rest (drop-while #(not= :sep %) words)))

(defn second-word [words]
  (first-word (rest-words words)))

(defn format 
  ([tag children]
   (conj [tag] (map parse-mfm children)))
  ([tag properties children]
   (conj [tag properties] (map parse-mfm children))))

(defn parse-mfm [in]
  (match in
    (seq :guard seq?)
      (format :span seq)
    {:type "text" :props {:text text}}
      (replace text "\n" "<br>")
    {:type "quote" :children children}
      (format :blockquote children)
    {:type "url" :props {:url url}}
      [:a {:href url} url]
    {:type "link" :props {:url url} :children children}
      (conj [:a {:href url}] (map parse-mfm children))
    {:type "bold" :children children}
      (format :b children)
    {:type "italic" :children children}
      (format :em children)
    {:type "small" :children children}
      (format :small children)
    {:type "center" :children children}
      (format :div 
              {:style "text-align:center;"}
              children)
    {:type "fn" :props {:name "ruby"}}
      (conj [:ruby]
            (let [words (sep-words in)]
                 (list (parse-mfm (first-word words))
                       (conj [:rt]
                             (parse-mfm (second-word words))))))
    :else [:span (str in)]))

(defn mfm->html [in]
  (hiccup-to-html
    (list
      [:html
       [:body
        (map (fn [node] 
                 (parse-mfm (walk/keywordize-keys node)))
             in)]])))

(defn init []
  (match (read-args)
    [:error msg] (js/console.error msg)
    [:run input output] 
      (.writeFileSync fs 
        output
        (mfm->html (js->clj
                  (.parse mfm 
                       (str (.readFileSync fs input))))))
    :else nil))
