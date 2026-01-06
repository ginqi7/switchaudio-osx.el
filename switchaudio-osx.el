;;; switchaudio-osx.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;; Custom Variables
(defcustom switchaudio-osx-command (executable-find "SwitchAudioSource")
  "")

;;; Inner Functions
(defun switchaudio-osx--to-string (device-info)
  (format "[%s][%s](%s)"
          (gethash "id" device-info)
          (gethash "type" device-info)
          (gethash "name" device-info)))

;;; API Functions
(defun switchaudio-osx-all ()
  "API: Get all audio devices"
  (json-parse-string
   (format
    "[%s]"
    (string-join
     (string-lines (shell-command-to-string (format "%s -a -f json" switchaudio-osx-command)))
     ","))
   :array-type 'list))

(defun switchaudio-osx-current ()
  "API: Get the current audio device.
Output is a list of hash map."
  (json-parse-string
   (shell-command-to-string (format "%s -c -f json" switchaudio-osx-command))))

(defun switchaudio-osx-active (device-info)
  "API: Active the device by DEVICE-INFO.
DEVICE-INFO is a hash map."
  (shell-command-to-string (format "%s -i %s -t %s"
                                   switchaudio-osx-command
                                   (gethash "id" device-info)
                                   (gethash "type" device-info))))

;;; Interactive Functions
(defun switchaudio-osx-list ()
  (interactive))

(defun switchaudio-osx-switch ()
  "Switch the currently active audio device interactively."
  (interactive)
  (let ((devices (switchaudio-osx-all))
        (hashmap (make-hash-table :test 'equal))
        (key))
    (dolist (device devices)
      (puthash (switchaudio-osx--to-string device) device hashmap))
    (setq key (completing-read "Switch to: " (hash-table-keys hashmap)))
    (switchaudio-osx-active (gethash key hashmap))))

(defun switchaudio-osx-mute ()
  (interactive))

(provide 'switchaudio-osx)
;;; switchaudio-osx.el ends here
