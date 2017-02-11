;;; test-helper --- Test helper for tco

;;; Code:

(require 'f)

(defvar tco-test--test-path
  (f-parent (f-this-file)))

(defvar tco-test--root-path
  (f-parent tco-test--test-path))

(require 'tco (f-expand "tco" tco-test--root-path))

(require 'undercover)
(undercover "tco.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
