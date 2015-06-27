#lang typed/racket

(require/typed/provide web-server/private/web-server-structs
                       [current-server-custodian (Parameterof Custodian)]
                       [make-servlet-custodian (-> Custodian)])

(require/typed/provide web-server/private/mime-types
                       [read-mime-types (-> Path-String (HashTable Symbol Bytes))]
                       [make-path->mime-type (-> Path-String (-> Path (Option Bytes)))])
