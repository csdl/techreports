(defun convert-project()
  "Converts a project-techreport.tex formatted file to project.tex format"
  (interactive)
  (beginning-of-buffer)
  (replace-string "\\subsubsection" "\\paragraph")
  (beginning-of-buffer)
  (replace-string "\\subsection" "\\subsubsection")
  (beginning-of-buffer)
  (replace-string "\\section" "\\subsection")
  )

(convert-project)
