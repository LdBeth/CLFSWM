(in-package :clfswm)

(leave-frame)
(select-previous-level)

(let ((frame (create-frame \:name \"Test root\" \:x 0.05 \:y 0.05)))
  (add-frame frame (current-child))
  (add-frame (create-frame \:name \"Test 1\" \:x 0.3 \:y 0 \:w 0.7 \:h 1) frame)
  (add-frame (create-frame \:name \"Test 2\" \:x 0 \:y 0 \:w 0.3 \:h 1) frame)
  (setf (current-child) (first (frame-child frame))))

(show-all-children *current-root*)



