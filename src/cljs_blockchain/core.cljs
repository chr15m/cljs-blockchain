(ns cljs-blockchain.core
    (:require
      ["bencode-js/lib/index" :as bencode]
      ["scrypt-js/scrypt" :as scrypt]
      [cljsjs.webtorrent :as wt]
      [cljsjs.nacl-fast :as nacl]
      [reagent.core :as r]))

(js/console.log (wt.))
(js/console.log (nacl.randomBytes 32))
(js/console.log (bencode/encode #js {:a 12}))
(scrypt (js/Uint8Array. [1 2 3 4]) (js/Uint8Array. ["a" "b" "c"]) 1024 8 1 32 js/console.log)

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to Reagent"]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
