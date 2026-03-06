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
  "Specifies the custom variable for the path to the SwitchAudioSource executable used by the switchaudio-osx package. It uses executable-find to locate the command in the system PATH.")

;;; Inner Functions
(defun switchaudio-osx--to-string (device-info)
  "Converts a device information hash table into a formatted string displaying the device's id, type, name, and an active indicator."
  (format "[%3s][%s](%s) %s"
          (gethash "id" device-info)
          (gethash "type" device-info)
          (gethash "name" device-info)
          (if (gethash "active" device-info) "*" "")))

(defclass switchaudio-osx:choices (transient-variable)
    ((choices     :initarg :choices)
     (fallback    :initarg :fallback    :initform nil)
     (default     :initarg :default     :initform nil))
    "Defines a class representing transient choices for switchaudio-osx operations. It extends the transient-variable class and includes initialization arguments for a list of choices, an optional fallback value, and an optional default value.")

(cl-defmethod transient-infix-read ((obj switchaudio-osx:choices))
  (let ((choices (oref obj choices)))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-infix-value ((obj switchaudio-osx:choices))
  "Return the value of OBJ's `value' slot if not nil,
     else return value of OBJ's `default' slot if not nil,
     else return nil"
  (let ((default (oref obj default)))
    (if-let ((value (oref obj value)))
        (concat (oref obj argument) value)
      (when default
        (concat (oref obj argument) default)))))

(cl-defmethod transient-format-value ((obj switchaudio-osx:choices))
  (let ((value (oref obj value))
        (choices (oref obj choices))
        (default  (oref obj default)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice value)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                choices
                (propertize "|" 'face 'transient-inactive-value))
     (and (or default)
          (concat
           (propertize "|" 'face 'transient-inactive-value)
           (cond
                 (default
                   (propertize (concat "default:" default)
                               'face
                               (if value
                                   'transient-inactive-value
                                 'transient-value))))))
     (propertize "]" 'face 'transient-inactive-value))))

(transient-define-infix transient-cycle-option ()
  :description "Option with list"
  :class 'switchaudio-osx:choices
  :shortarg "t"
  :argument ""
  :choices '("input" "output" "system" "all")
  :default "all")

;;; API Functions
(defun switchaudio-osx-all (&optional type)
  "API: Get all audio devices"
  (let ((data (json-parse-string
               (format
                "[%s]"
                (string-join
                 (string-lines (shell-command-to-string (format "%s -a -f json -t %s"
                                                                switchaudio-osx-command
                                                                (or type "all"))))
                 ","))
               :array-type 'list))
        (active-input-id (gethash "id" (switchaudio-osx-current "input")))
        (active-output-id (gethash "id" (switchaudio-osx-current "output"))))
    (mapc
     (lambda (item) (when (or (and (string= (gethash "id" item) active-input-id)
                               (string= (gethash "type" item) "input"))
                           (and (string= (gethash "id" item) active-output-id)
                            (string= (gethash "type" item) "output")))
                         (puthash "active" t item)))

     data)))

(switchaudio-osx-all)

(defun switchaudio-osx-current (&optional type)
  "API: Get the current audio device.
Output is a list of hash map."
  (json-parse-string
   (shell-command-to-string (format "%s -c -f json -t %s"
                                    switchaudio-osx-command
                                    (or type all)))))

(defun switchaudio-osx-active (device-info)
  "API: Active the device by DEVICE-INFO.
DEVICE-INFO is a hash map."
  (shell-command-to-string (format "%s -i %s -t %s"
                                   switchaudio-osx-command
                                   (gethash "id" device-info)
                                   (gethash "type" device-info))))

;;; Interactive Functions
(transient-define-prefix switchaudio-osx ()
  "Provides an interactive transient command prefix for managing audio settings on OS X. It includes an argument section for cycling through audio types and two command options: one to list available audio devices and another to switch the current audio device."
  ["Arguments"
     ("t" "type" transient-cycle-option)]
  ["Switchaudio"
   ("l" "List" switchaudio-osx-list)
   ("s" "Switch" switchaudio-osx-switch)])

(defun switchaudio-osx-list (&optional args)
  "Add switchaudio-osx utilities to list and switch macOS audio devices via SwitchAudioSource"
  (interactive (list (transient-args 'switchaudio-osx)))
  (with-current-buffer (get-buffer-create "*switchaudio-osx-list*")
    (let* ((buffer-read-only nil)
           (column-model ; column model
            (list (make-ctbl:cmodel
                   :title "ID" :sorter 'ctbl:sort-number-lessp
                   :min-width 5 :align 'right)
                  (make-ctbl:cmodel
                   :title "Name" :align 'center
                   :sorter (lambda (a b) (ctbl:sort-number-lessp (length a) (length b))))
                  (make-ctbl:cmodel
                   :title "Type" :align 'left)
                  (make-ctbl:cmodel
                   :title "Active" :align 'center)))
           (type (car args))
           (hash (switchaudio-osx-all type))
           (active-input (switchaudio-osx-current "input"))
           (active-output (switchaudio-osx-current "output"))
           (data
            (mapcar (lambda (item) (list (gethash "id" item)
                                     (gethash "name" item)
                                     (gethash "type" item)
                                     (if (gethash "active" item) "*" "")
                                     (gethash "uuid" item)))
                    hash))
           (model ; data model
            (make-ctbl:model
             :column-model column-model :data data))
           (component)) ; ctable component

      (erase-buffer)
      (setq component (ctbl:create-table-component-region :model model))
      (pop-to-buffer (ctbl:cp-get-buffer component)))
    (setq-local buffer-read-only t)))

(defun switchaudio-osx-switch (&optional args)
  "Switch the currently active audio device interactively."
  (interactive (list (transient-args 'switchaudio-osx)))
  (let ((devices (switchaudio-osx-all (car args)))
        (hashmap (make-hash-table :test 'equal))
        (key))
    (dolist (device devices)
      (puthash (switchaudio-osx--to-string device) device hashmap))
    (setq key (completing-read "Switch to: " (hash-table-keys hashmap) nil nil))
    (switchaudio-osx-active (gethash key hashmap))))

(defun switchaudio-osx-mute ()
  (interactive))

(provide 'switchaudio-osx)
;;; switchaudio-osx.el ends here
