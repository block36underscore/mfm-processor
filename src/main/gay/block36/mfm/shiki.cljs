(ns gay.block36.mfm.shiki
    (:require 
      ["shiki" :as shiki]))

(defn shiki-server []
  (fn [msg]
    (let [data (js->clj msg)]
      (shiki/codeToHtml (get data "code")
                        #js{:lang  (get data "lang")
                            :theme (or (get data "theme")
                                       "catppuccin-mocha")}))))
