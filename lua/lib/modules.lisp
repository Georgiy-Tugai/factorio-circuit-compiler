(in-package :lua-runtime)
(cl-interpol:enable-interpol-syntax)
(defvar lua::module-table
  (lua-table-constructor `(("_G" . ,lua::|_G|))))
(defvar lua::preload-table
  (lua-table-constructor nil))
(lua-defvar |package| (lua-table-constructor
                       `(("config" . ,#?"${(uiop/pathname:directory-separator-for-host)}\n;\n?\n!\n-\n")
                         ("cpath" . "")
                         ("loaded" . ,lua::module-table)
                         ("path" . "")
                         ("preload" . ,lua::preload-table)
                         ;; TODO: Stub or implement the other four
                         ;; loaders (package.path, package.cpath, AIO)
                         ("searchers" . ,(lua-table-constructor
                                          `((nil . ,(alexandria:named-lambda
                                                        search-preload-table (name)
                                                      (lua-index lua::preload-table name))))))
                         ;; TODO
                         ("searchpath" . lua-nil)
                         )))
(setf (lua-index lua::module-table "package")
      (lua-index lua::|_G| "package"))
