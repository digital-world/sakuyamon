#lang typed/racket

(require/typed/provide file/md5
                       [md5 (->* {(U String Bytes Input-Port)} {Boolean} Bytes)])