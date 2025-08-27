(ns gay.block36.mfm.main
  ;(:refer-clojure :exclude [await])
  (:require
   [clojure.string :refer [ends-with? replace split]]
   [clojure.core.match :refer [match]]
   ["node:fs" :as fs]
   ["mfm-js" :as mfm]
   ["shiki" :as shiki]
   ["sync-rpc" :as rpc]
   [hickory.render :refer [hiccup-to-html]]
   [clojure.walk :as walk]
   [promesa.core :as p]))

(def shiki-processor (rpc (str js/__dirname "/shiki-server.js")))

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
    {:type "unicodeEmoji"
     :props {:emoji emoji}}
      emoji
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
    {:type "strike" :children children}
      (format :s children)
    {:type "center" :children children}
      (format :div 
              {:style "text-align:center;"}
              children)
    {:type "inlineCode" :props {:code code}}
      (conj [:code]
            code)
    {:type "blockCode" :props {:code code
                               :lang lang}}
      (conj [:div] (shiki-processor #js{:code  code
                                       :lang  lang
                                       :theme "catppuccin-mocha"}))
    {:type "fn"
     :props {:name "flip" 
             :args {:h true
                    :v true}}
     :children children}
      (format :span
              {:style "display: inline-block; transform: scale(-1,-1);"}
              children)
    {:type "fn"
     :props {:name "flip"
             :args {:v true}}
     :children children}
      (format :span
              {:style "display: inline-block; transform: scaleY(-1);"}
              children)
    {:type "fn"
     :props {:name "flip"}
     :children children}
      (format :span
              {:style "display: inline-block; transform: scaleX(-1);"}
              children)
    {:type "fn"
     :props {:name "font"
             :args args}
     :children children}
      (format :span
              {:style (str "display: inline-block; font-family: "
                           (name (key (first args)))
                           ";")}
              children)
    {:type "fn"
     :props {:name "ruby"}}
      (conj [:ruby]
            (let [words (sep-words in)]
                 (list (parse-mfm (first-word words))
                       (conj [:rt]
                             (parse-mfm (second-word words))))))
    {:type "fn"
     :props {:name "blur"}
     :children children}
      (format :span
              {:class "_mfm_blur_"}
              children)
    {:type "fn"
     :props {:name (name :guard 
                           #(contains? #{"fg" "bg"}
                                       %))
             :args {:color color}}
     :children children}
    (format :span
            {:style (str (if (= name "fg")
                             "color: "
                             "background-color: ")
                         color
                         ";")}
            children)
    {:type "fn"
     :props {:name "border" :args args}
     :children children}
    (let [{style  :style
           width  :width
           color  :color
           radius :radius
           noclip :noclip}
            (merge {:style  "solid"
                    :width  "1"
                    :color  "000"
                    :radius "0"
                    :noclip false} args)]
      (format :span 
              {:style (str "border: "
                           width
                           "px "
                           style
                           " #"
                           color
                           "; border-radius:"
                           radius
                           "px; "
                           (if noclip
                               ""
                               "overflow: clip;"))}
              children))
    {:type "fn"
     :props {:name "rotate"
             :args {:deg deg}}
     :children children}
      (format :span
              {:style (str "display: inline-block; transform: rotate("
                           deg
                           "deg);  transform-origin: center center;")}
              children)
    {:type "fn"
     :props {:name "position"
             :args args}
     :children children}
      (let [{x :x
             y :y}
              (merge {:x 0
                      :y 0}
                     args)]
        (format :span
                {:style (str "display: inline-block; transform: translateX("
                             x
                             "em) translateY("
                             y
                             "em);")}
                children))
    {:type "fn"
     :props {:name "scale"
             :args args}
     :children children}
      (let [{x :x
               y :y}
                (merge {:x 1
                        :y 1}
                       args)]
        (format :span
                {:style (str "display: inline-block; transform: scale("
                             x
                             ", "
                             y
                             ");")}
                children))
    {:type "fn"
     :props {:name (name :guard
                           #(contains? #{"x2"
                                         "x3"
                                         "x4"}
                                       %))}
     :children children}
      (format :span
              {:style (str "font-size:"
                           (match name
                                  "x2" "200%"
                                  "x3" "400%"
                                  "x4" "600%")
                           ";")}
              children)
    {:type "search"
     :props {:query query}}
      [:div {:class "_mfm_search_box"}
        [:input {:type "search"
                 :class "_mfm_search_input_"
                 :value query}]
        [:button {:class "_mfm_search_button_"}
                 "search"]]
    :else [:span (str in)]))

(defn mfm->html [in]
  (hiccup-to-html
    (list
      [:html
        [:head
          [:link {:rel "stylesheet" 
                  :href "style.css"}]]
        [:body
          (map (fn [node] 
                   (parse-mfm (walk/keywordize-keys node)))
               in)]])))

(defn init []
  (println (.-langs shiki))
  (match (read-args)
    [:error msg] (js/console.error msg)
    [:run input output] 
      (.writeFileSync fs 
        output
        (mfm->html (js->clj
                  (.parse mfm 
                       (str (.readFileSync fs input))))))
    :else nil))
