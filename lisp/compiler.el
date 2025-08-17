;;; compiler.el --- Emacs compiler inspired by compiler.nvim -*- lexical-binding: t; -*-

;;; Commentary:
;; Exact port of compiler.nvim architecture to Emacs
;; Maintains the same file structure, naming conventions, and behavior

;;; Code:

(require 'compile)
(require 'files)

;;; === COMPILER UTILS (equivalent to lua/compiler/utils.lua) ===

(defun compiler-utils-os-path (path &optional quoted)
  "Convert PATH to OS format, optionally QUOTED like utils.os_path()."
  (let ((normalized-path (expand-file-name path)))
    (if quoted
        (shell-quote-argument normalized-path)
      normalized-path)))

(defun compiler-utils-get-cwd ()
  "Get current working directory like vim.fn.getcwd()."
  default-directory)

(defun compiler-utils-find-files-to-compile (entry-point pattern &optional recursive)
  "Find files matching PATTERN relative to ENTRY-POINT, like utils.find_files_to_compile()."
  (let* ((entry-dir (file-name-directory entry-point))
         (files (if recursive
                   (directory-files-recursively entry-dir pattern)
                 (directory-files entry-dir t pattern))))
    ;; Filter out directories
    (setq files (seq-filter #'file-regular-p files))
    ;; Return as space-separated quoted string exactly like original
    (mapconcat (lambda (f) (compiler-utils-os-path f t)) files " ")))

(defun compiler-utils-get-solution-file ()
  "Get .solution.toml file path if exists like utils.get_solution_file()."
  (let ((solution-path (expand-file-name ".solution.toml" (compiler-utils-get-cwd))))
    (when (file-exists-p solution-path)
      solution-path)))

(defun compiler-utils-parse-solution-file (file-path)
  "Parse .solution.toml FILE-PATH like utils.parse_solution_file()."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((config '())
            (current-section nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties 
                                   (line-beginning-position) (line-end-position)))))
            (cond
             ;; Section header [SectionName]
             ((string-match "\\[\\([^]]+\\)\\]" line)
              (setq current-section (match-string 1 line)))
             ;; Key = value pairs
             ((and current-section (string-match "\\([^=]+\\)\\s-*=\\s-*\"?\\([^\"]*\\)\"?" line))
              (let ((key (string-trim (match-string 1 line)))
                    (value (string-trim (match-string 2 line))))
                (push (cons current-section (cons (cons key value) 
                                                 (cdr (assoc current-section config)))) 
                      (delq (assoc current-section config) config)))))
          (forward-line)))
        config))))

;;; === LANGUAGE MODULES (exact copy of lua/compiler/languages/*.lua structure) ===

;;; C++ Language Module (lua/compiler/languages/cpp.lua)
(defvar compiler-cpp-options
  '(("Build and run program" . "option1")
    ("Build program" . "option2")
    ("Run program" . "option3") 
    ("Build solution" . "option4"))
  "Frontend - options displayed on telescope.")

(defun compiler-cpp-action (selected-option)
  "Backend - overseer tasks performed on option selected."
  (let* ((entry-point (compiler-utils-os-path 
                       (concat (compiler-utils-get-cwd) "main.cpp")))
         (files (compiler-utils-find-files-to-compile entry-point "\\.cpp\\'" t))
         (output-dir (compiler-utils-os-path 
                      (concat (compiler-utils-get-cwd) "bin/")))
         (output (compiler-utils-os-path 
                  (concat (compiler-utils-get-cwd) "bin/program")))
         (arguments "-Wall -g")
         (final-message "--task finished--"))
    
    ;; Debug info about found files
    (if (string-empty-p files)
        (message "⚠️ No %s files found in %s" 
                 (if (string-match-p "cpp" entry-point) "C++" "C")
                 (file-name-directory entry-point))
      (message "🔍 Found files: %s" 
               (replace-regexp-in-string (regexp-quote (compiler-utils-get-cwd)) "./" files)))
    
    ;; Fallback to current file if no files found (like original)
    (when (string-empty-p files)
      (when-let ((current-file (buffer-file-name)))
        (when (string-match-p (if (string-match-p "cpp" entry-point) "\\.cpp\\'" "\\.c\\'") current-file)
          (setq files (compiler-utils-os-path current-file t))
          (message "📝 Using current file as fallback"))))
    
    ;; Prevent execution if no source files
    (when (string-empty-p files)
      (user-error "❌ No source files found to compile. Please ensure you have .cpp/.c files in the project directory"))
    
    (cond
     ((string= selected-option "option1") ; Build and run program
      (compiler--new-task
       "- C++ compiler"
       (list (list :name (format "- Build & run program → \"%s\"" entry-point)
                   :cmd (format "(rm -f \"%s\" 2>/dev/null; mkdir -p \"%s\" 2>/dev/null; g++ %s -o \"%s\" %s) && \"%s\""
                               output output-dir files output arguments output)
                   :components '("default_extended")))))
     
     ((string= selected-option "option2") ; Build program
      (compiler--new-task
       "- C++ compiler"
       (list (list :name (format "- Build program → \"%s\"" entry-point)
                   :cmd (format "(rm -f \"%s\" 2>/dev/null; mkdir -p \"%s\" 2>/dev/null; g++ %s -o \"%s\" %s) && echo '✅ Build completed successfully'"
                               output output-dir files output arguments)
                   :components '("default_extended")))))
     
     ((string= selected-option "option3") ; Run program
      ;; Check if executable exists before trying to run
      (if (file-exists-p output)
          (compiler--new-task
           "- C++ compiler"
           (list (list :name (format "- Run program → \"%s\"" output)
                       :cmd (format "echo '--- Program Output ---' && \"%s\""
                                   output)
                       :components '("default_extended"))))
        (message "❌ Executable not found: %s. Please build first with 'Build program'." output)))
     
     ((string= selected-option "option4") ; Build solution
      (compiler-cpp--build-solution)))))

(defun compiler-cpp--build-solution ()
  "Handle solution building for C++ (option4)."
  (let ((solution-file (compiler-utils-get-solution-file)))
    (if solution-file
        (let ((config (compiler-utils-parse-solution-file solution-file)))
          (message "Building C++ solution with %d entries..." (length config))
          ;; TODO: Implement full solution logic like in original
          )
      (message "No .solution.toml file found"))))

;;; C Language Module (lua/compiler/languages/c.lua)
(defvar compiler-c-options
  '(("Build and run program" . "option1")
    ("Build program" . "option2")
    ("Run program" . "option3")
    ("Build solution" . "option4"))
  "Frontend - options displayed on telescope.")

(defun compiler-c-action (selected-option)
  "Backend - overseer tasks performed on option selected."
  (let* ((entry-point (compiler-utils-os-path 
                       (concat (compiler-utils-get-cwd) "main.c")))
         (files (compiler-utils-find-files-to-compile entry-point "\\.c\\'" t))
         (output-dir (compiler-utils-os-path 
                      (concat (compiler-utils-get-cwd) "bin/")))
         (output (compiler-utils-os-path 
                  (concat (compiler-utils-get-cwd) "bin/program")))
         (arguments "-Wall -g")
         (final-message "--task finished--"))
    
    ;; Debug info and fallback logic
    (if (string-empty-p files)
        (message "⚠️ No C files found in %s" (file-name-directory entry-point))
      (message "🔍 Found C files: %s" 
               (replace-regexp-in-string (regexp-quote (compiler-utils-get-cwd)) "./" files)))
    
    ;; Fallback logic
    (when (string-empty-p files)
      (when-let ((current-file (buffer-file-name)))
        (when (string-match-p "\\.c\\'" current-file)
          (setq files (compiler-utils-os-path current-file t))
          (message "📝 Using current file as fallback"))))
    
    ;; Prevent execution if no source files
    (when (string-empty-p files)
      (user-error "❌ No source files found to compile. Please ensure you have .c files in the project directory"))
    
    (cond
     ((string= selected-option "option1")
      (compiler--new-task
       "- C compiler"
       (list (list :name (format "- Build & run program → %s" entry-point)
                   :cmd (format "(rm -f %s 2>/dev/null; mkdir -p %s 2>/dev/null; gcc %s -o %s %s) && %s"
                               output output-dir files output arguments output)
                   :components '("default_extended")))))
     
     ((string= selected-option "option2")
      (compiler--new-task
       "- C compiler"
       (list (list :name (format "- Build program → %s" entry-point)
                   :cmd (format "(rm -f %s 2>/dev/null; mkdir -p %s 2>/dev/null; gcc %s -o %s %s) && echo '✅ Build completed successfully'"
                               output output-dir files output arguments)
                   :components '("default_extended")))))
     
     ((string= selected-option "option3") ; Run program
      ;; Check if executable exists before trying to run  
      (if (file-exists-p output)
          (compiler--new-task
           "- C compiler"
           (list (list :name (format "- Run program → %s" output)
                       :cmd (format "echo '--- Program Output ---' && %s"
                                   output)
                       :components '("default_extended"))))
        (message "❌ Executable not found: %s. Please build first with 'Build program'." output))))))

;;; Python Language Module (lua/compiler/languages/python.lua)
(defvar compiler-python-options
  '(("Run this file" . "option1")
    ("Run program" . "option2")
    ("Run solution" . "option3"))
  "Frontend - options displayed on telescope.")

(defun compiler-python-action (selected-option)
  "Backend - overseer tasks performed on option selected."
  (let* ((current-file (compiler-utils-os-path (buffer-file-name) t))
         (entry-point (compiler-utils-os-path 
                       (concat (compiler-utils-get-cwd) "main.py") t))
         (arguments "")
         (final-message "--task finished--"))
    
    (cond
     ((string= selected-option "option1") ; Run this file
      (compiler--new-task
       "- Python interpreter"
       (list (list :name (format "- Run this file → %s" current-file)
                   :cmd (format "python3 %s"
                               current-file)
                   :components '("default_extended")))))
     
     ((string= selected-option "option2") ; Run program
      (compiler--new-task
       "- Python interpreter"
       (list (list :name (format "- Run program → %s" entry-point)
                   :cmd (format "python3 %s"
                               entry-point)
                   :components '("default_extended")))))
     
     ((string= selected-option "option3") ; Run solution
      (message "Python solution running not implemented yet")))))

;;; Go Language Module
(defvar compiler-go-options
  '(("Build and run program" . "option1")
    ("Build program" . "option2")
    ("Run program" . "option3")
    ("Build solution" . "option4"))
  "Frontend - options displayed on telescope.")

(defun compiler-go-action (selected-option)
  "Backend - overseer tasks performed on option selected."
  (let* ((entry-point (compiler-utils-os-path 
                       (concat (compiler-utils-get-cwd) "main.go")))
         (files (compiler-utils-find-files-to-compile entry-point "\\.go\\'" t))
         (output-dir (compiler-utils-os-path 
                      (concat (compiler-utils-get-cwd) "bin/")))
         (output (compiler-utils-os-path 
                  (concat (compiler-utils-get-cwd) "bin/program")))
         (arguments "-a -gcflags='-N -l'")
         (final-message "--task finished--"))
    
    ;; Debug and fallback logic
    (if (string-empty-p files)
        (message "⚠️ No Go files found in %s" (file-name-directory entry-point))
      (message "🔍 Found Go files: %s" 
               (replace-regexp-in-string (regexp-quote (compiler-utils-get-cwd)) "./" files)))
    
    (when (string-empty-p files)
      (when-let ((current-file (buffer-file-name)))
        (when (string-match-p "\\.go\\'" current-file)
          (setq files (compiler-utils-os-path current-file t))
          (message "📝 Using current file as fallback"))))
    
    (when (string-empty-p files)
      (user-error "❌ No Go source files found to compile"))
    
    (cond
     ((string= selected-option "option1") ; Build and run
      (compiler--new-task
       "- Go compiler"
       (list (list :name (format "- Build & run program → %s" entry-point)
                   :cmd (format "(rm -f %s 2>/dev/null; mkdir -p %s 2>/dev/null; go build %s -o %s %s) && %s"
                               output output-dir arguments output files output)
                   :components '("default_extended")))))
     
     ((string= selected-option "option2") ; Build only
      (compiler--new-task
       "- Go compiler"
       (list (list :name (format "- Build program → %s" entry-point)
                   :cmd (format "(rm -f %s 2>/dev/null; mkdir -p %s 2>/dev/null; go build %s -o %s %s) && echo '✅ Build completed successfully'"
                               output output-dir arguments output files)
                   :components '("default_extended")))))
     
     ((string= selected-option "option3") ; Run only
      (if (file-exists-p output)
          (compiler--new-task
           "- Go compiler"
           (list (list :name (format "- Run program → %s" output)
                       :cmd (format "echo '--- Program Output ---' && %s"
                                   output)
                       :components '("default_extended"))))
        (message "❌ Executable not found: %s. Please build first." output))))))

;;; Java Language Module
(defvar compiler-java-options
  '(("Build and run program (class)" . "option1")
    ("Build program (class)" . "option2")
    ("Run program (class)" . "option3")
    ("Build solution (class)" . "option4"))
  "Frontend - options displayed on telescope.")

(defun compiler-java-action (selected-option)
  "Backend - overseer tasks performed on option selected."
  (let* ((entry-point (compiler-utils-os-path 
                       (concat (compiler-utils-get-cwd) "Main.java")))
         (files (compiler-utils-find-files-to-compile entry-point "\\.java\\'" t))
         (output-dir (compiler-utils-os-path 
                      (concat (compiler-utils-get-cwd) "bin/")))
         (output-filename "Main")
         (arguments "-Xlint:all")
         (final-message "--task finished--"))
    
    ;; Debug and fallback logic
    (if (string-empty-p files)
        (message "⚠️ No Java files found in %s" (file-name-directory entry-point))
      (message "🔍 Found Java files: %s" 
               (replace-regexp-in-string (regexp-quote (compiler-utils-get-cwd)) "./" files)))
    
    (when (string-empty-p files)
      (when-let ((current-file (buffer-file-name)))
        (when (string-match-p "\\.java\\'" current-file)
          (setq files (compiler-utils-os-path current-file t))
          (message "📝 Using current file as fallback"))))
    
    (when (string-empty-p files)
      (user-error "❌ No Java source files found to compile"))
    
    (cond
     ((string= selected-option "option1") ; Build and run
      (compiler--new-task
       "- Java compiler"
       (list (list :name (format "- Build & run program (class) → %s" entry-point)
                   :cmd (format "(rm -f %s*.class 2>/dev/null; mkdir -p %s 2>/dev/null; javac %s -d %s %s) && java -cp %s %s"
                               output-dir output-dir files output-dir arguments output-dir output-filename)
                   :components '("default_extended")))))
     
     ((string= selected-option "option2") ; Build only
      (compiler--new-task
       "- Java compiler"
       (list (list :name (format "- Build program (class) → %s" entry-point)
                   :cmd (format "(rm -f %s*.class 2>/dev/null; mkdir -p %s 2>/dev/null; javac %s -d %s %s) && echo '✅ Build completed successfully'"
                               output-dir output-dir files output-dir arguments)
                   :components '("default_extended")))))
     
     ((string= selected-option "option3") ; Run only
      (let ((class-file (expand-file-name (concat output-filename ".class") output-dir)))
        (if (file-exists-p class-file)
            (compiler--new-task
             "- Java compiler"
             (list (list :name (format "- Run program (class) → %s.class" output-filename)
                         :cmd (format "echo '--- Program Output ---' && java -cp %s %s"
                                     output-dir output-filename)
                         :components '("default_extended"))))
          (message "❌ Class file not found: %s. Please build first." class-file)))))))

;;; JavaScript/Node.js Language Module
(defvar compiler-javascript-options
  '(("Run this file" . "option1")
    ("Run program" . "option2")
    ("Run solution" . "option3"))
  "Frontend - options displayed on telescope.")

(defun compiler-javascript-action (selected-option)
  "Backend - overseer tasks performed on option selected."
  (let* ((current-file (compiler-utils-os-path (buffer-file-name) t))
         (entry-point (compiler-utils-os-path 
                       (concat (compiler-utils-get-cwd) "main.js") t))
         (index-point (compiler-utils-os-path 
                       (concat (compiler-utils-get-cwd) "index.js") t)))
    
    (cond
     ((string= selected-option "option1") ; Run this file
      (compiler--new-task
       "- Node.js interpreter"
       (list (list :name (format "- Run this file → %s" current-file)
                   :cmd (format "node %s" current-file)
                   :components '("default_extended")))))
     
     ((string= selected-option "option2") ; Run program
      (let ((target (if (file-exists-p (substring entry-point 1 -1)) entry-point index-point)))
        (compiler--new-task
         "- Node.js interpreter"
         (list (list :name (format "- Run program → %s" target)
                     :cmd (format "node %s" target)
                     :components '("default_extended"))))))
     
     ((string= selected-option "option3") ; Run solution
      (message "JavaScript solution running not implemented yet")))))

;;; Rust Language Module
(defvar compiler-rust-options
  '(("Build and run program" . "option1")
    ("Build program" . "option2") 
    ("Run program" . "option3")
    ("Build solution" . "option4"))
  "Frontend - options displayed on telescope.")

(defun compiler-rust-action (selected-option)
  "Backend - overseer tasks performed on option selected."
  (let* ((cargo-file (expand-file-name "Cargo.toml" (compiler-utils-get-cwd))))
    
    (if (file-exists-p cargo-file)
        ;; Cargo project
        (cond
         ((string= selected-option "option1") ; Build and run
          (compiler--new-task
           "- Rust compiler (Cargo)"
           (list (list :name "- Cargo build & run"
                       :cmd "cargo run"
                       :components '("default_extended")))))
         
         ((string= selected-option "option2") ; Build only
          (compiler--new-task
           "- Rust compiler (Cargo)"
           (list (list :name "- Cargo build"
                       :cmd "cargo build && echo '✅ Build completed successfully'"
                       :components '("default_extended")))))
         
         ((string= selected-option "option3") ; Run only
          (compiler--new-task
           "- Rust compiler (Cargo)"
           (list (list :name "- Cargo run"
                       :cmd "echo '--- Program Output ---' && cargo run"
                       :components '("default_extended"))))))
      
      ;; Single file Rust
      (let* ((entry-point (compiler-utils-os-path 
                           (concat (compiler-utils-get-cwd) "main.rs")))
             (output-dir (compiler-utils-os-path 
                          (concat (compiler-utils-get-cwd) "bin/")))
             (output (compiler-utils-os-path 
                      (concat (compiler-utils-get-cwd) "bin/program")))
             (current-file (buffer-file-name)))
        
        (when (and (not (file-exists-p entry-point)) current-file
                   (string-match-p "\\.rs\\'" current-file))
          (setq entry-point (compiler-utils-os-path current-file t)))
        
        (cond
         ((string= selected-option "option1") ; Build and run
          (compiler--new-task
           "- Rust compiler"
           (list (list :name (format "- Build & run program → %s" entry-point)
                       :cmd (format "(rm -f %s 2>/dev/null; mkdir -p %s 2>/dev/null; rustc %s -o %s) && %s"
                                   output output-dir entry-point output output)
                       :components '("default_extended")))))
         
         ((string= selected-option "option2") ; Build only
          (compiler--new-task
           "- Rust compiler"
           (list (list :name (format "- Build program → %s" entry-point)
                       :cmd (format "(rm -f %s 2>/dev/null; mkdir -p %s 2>/dev/null; rustc %s -o %s) && echo '✅ Build completed successfully'"
                                   output output-dir entry-point output)
                       :components '("default_extended"))))))))))

;;; === BUILD AUTOMATION UTILITIES (lua/compiler/bau/*.lua) ===

(defvar compiler--bau-detectors
  '((make . ("Makefile" "makefile"))
    (cmake . ("CMakeLists.txt"))
    (gradle . ("build.gradle" "build.gradle.kts"))
    (maven . ("pom.xml"))
    (npm . ("package.json"))
    (cargo . ("Cargo.toml")))
  "BAU file detectors.")

(defun compiler--detect-bau ()
  "Detect available build automation utilities."
  (let ((detected '())
        (cwd (compiler-utils-get-cwd)))
    (dolist (bau compiler--bau-detectors)
      (dolist (file (cdr bau))
        (when (file-exists-p (expand-file-name file cwd))
          (push (car bau) detected))))
    detected))

(defun compiler--get-bau-options (bau-type)
  "Get options for BAU-TYPE."
  (pcase bau-type
    ('make '(("Make: all" . "all") ("Make: build" . "build") ("Make: clean" . "clean")))
    ('cmake '(("CMake: build" . "build") ("CMake: clean" . "clean")))
    ('gradle '(("Gradle: build" . "build") ("Gradle: run" . "run") ("Gradle: clean" . "clean")))
    ('maven '(("Maven: compile" . "compile") ("Maven: package" . "package") ("Maven: clean" . "clean")))
    ('npm '(("NPM: start" . "start") ("NPM: build" . "build") ("NPM: test" . "test")))
    ('cargo '(("Cargo: build" . "build") ("Cargo: run" . "run") ("Cargo: test" . "test")))))

;;; === CORE SYSTEM (lua/compiler/init.lua equivalent) ===

(defvar compiler--language-modules
  '((c . (compiler-c-options . compiler-c-action))
    (cpp . (compiler-cpp-options . compiler-cpp-action))
    (c++ . (compiler-cpp-options . compiler-cpp-action))
    (python . (compiler-python-options . compiler-python-action))
    (go . (compiler-go-options . compiler-go-action))
    (java . (compiler-java-options . compiler-java-action))
    (javascript . (compiler-javascript-options . compiler-javascript-action))
    (js . (compiler-javascript-options . compiler-javascript-action))
    (rust . (compiler-rust-options . compiler-rust-action)))
  "Language module registry.")

(defvar compiler--last-selected nil
  "Last selected option for redo functionality.")

(defun compiler--detect-filetype ()
  "Detect current buffer filetype like vim filetype detection."
  (when-let ((filename (buffer-file-name)))
    (let ((ext (file-name-extension filename)))
      (cond
       ((member ext '("c")) 'c)
       ((member ext '("cpp" "cc" "cxx" "c++")) 'cpp)
       ((member ext '("py")) 'python)
       ((member ext '("go")) 'go)
       ((member ext '("js")) 'javascript)
       ((member ext '("java")) 'java)
       ((member ext '("rs")) 'rust)
       ((member ext '("lua")) 'lua)
       ((member ext '("sh" "bash")) 'shell)))))

(defun compiler--get-options ()
  "Get all available options for telescope picker."
  (let ((options '())
        (filetype (compiler--detect-filetype))
        (bau-list (compiler--detect-bau)))
    
    ;; Add BAU options first (highest priority like original)
    (dolist (bau bau-list)
      (setq options (append options (compiler--get-bau-options bau))))
    
    ;; Add language-specific options
    (when filetype
      (when-let ((lang-config (alist-get filetype compiler--language-modules)))
        (setq options (append options (symbol-value (car lang-config))))))
    
    ;; Add solution options if .solution.toml exists
    (when (compiler-utils-get-solution-file)
      (setq options (append options '(("Build solution" . "solution")))))
    
    options))

(defun compiler--new-task (task-name tasks)
  "Create new task equivalent to overseer.new_task()."
  (let* ((first-task (car tasks))
         (task-title (plist-get first-task :name))
         (command (plist-get first-task :cmd))
         (buffer-name (format "*%s*" task-name)))
    
    ;; Save buffer if modified (like original)
    (when (and (buffer-file-name) (buffer-modified-p))
      (save-buffer))
    
    ;; Start compilation
    (let ((compilation-buffer-name-function (lambda (_) buffer-name)))
      (compile command))
    
    ;; Clean up the output after a short delay
    (run-at-time "1 sec" nil
                 (lambda ()
                   (when-let ((comp-buffer (get-buffer buffer-name)))
                     (with-current-buffer comp-buffer
                       (let ((inhibit-read-only t))
                         (goto-char (point-min))
                         ;; Remove the compilation mode line
                         (when (re-search-forward "^-\\*-.*-\\*-$" nil t)
                           (delete-region (line-beginning-position) (1+ (line-end-position))))
                         ;; Remove "Compilation started" line
                         (goto-char (point-min))
                         (when (re-search-forward "^Compilation started.*$" nil t)
                           (delete-region (line-beginning-position) (1+ (line-end-position))))
                         ;; Remove the command line (anything with rm/mkdir/long paths)
                         (goto-char (point-min))
                         (when (re-search-forward "^(rm -f.*$" nil t)
                           (delete-region (line-beginning-position) (1+ (line-end-position))))
                         ;; Remove "Compilation finished" line at the end
                         (goto-char (point-max))
                         (when (re-search-backward "^Compilation finished.*$" nil t)
                           (delete-region (line-beginning-position) (point-max))))))))
    
    ;; Show buffer (equivalent to task:start() + OverseerOpen)
    (when-let ((comp-buffer (get-buffer buffer-name)))
      (display-buffer comp-buffer))
    
    ;; Store for redo
    (setq compiler--last-selected (list task-name tasks))))

;;; === PUBLIC COMMANDS (equivalent to compiler.nvim commands) ===

;;;###autoload
(defun compiler-open ()
  "Main command equivalent to :CompilerOpen.
Opens telescope picker with compilation options."
  (interactive)
  (let ((options (compiler--get-options)))
    (if options
        (let* ((choices (mapcar #'car options))
               (selected (completing-read "Compiler: " choices nil t)))
          (when selected
            (let ((option-value (alist-get selected options nil nil #'string=)))
              (compiler--execute-option selected option-value))))
      (message "No compilation options available"))))

(defun compiler--execute-option (option-text option-value)
  "Execute selected option."
  (cond
   ;; BAU actions (contain ":")
   ((string-match "\\([^:]+\\):\\s-*\\(.+\\)" option-text)
    (let ((bau-name (downcase (match-string 1 option-text)))
          (action (match-string 2 option-text)))
      (compiler--run-bau-action (intern bau-name) action)))
   
   ;; Solution actions
   ((string= option-value "solution")
    (message "Solution building"))
   
   ;; Language-specific actions
   (t
    (let ((filetype (compiler--detect-filetype)))
      (when filetype
        (when-let ((lang-config (alist-get filetype compiler--language-modules)))
          (funcall (cdr lang-config) option-value)))))))

(defun compiler--run-bau-action (bau-type action)
  "Run BAU action."
  (let ((cmd (pcase bau-type
               ('make (format "make %s" action))
               ('cmake (if (string= action "build") 
                          "cmake --build build" 
                        (format "cmake %s" action)))
               ('gradle (format "./gradlew %s" action))
               ('maven (format "mvn %s" action))
               ('npm (format "npm %s" action))
               ('cargo (format "cargo %s" action)))))
    (compiler--new-task (format "- %s" bau-type) 
                       (list (list :name (format "- %s %s" bau-type action)
                                  :cmd cmd
                                  :components '("default_extended"))))))

;;;###autoload
(defun compiler-redo ()
  "Redo last compilation - equivalent to :CompilerRedo."
  (interactive)
  (if compiler--last-selected
      (apply #'compiler--new-task compiler--last-selected)
    (message "No previous compilation to redo")))

;;;###autoload 
(defun compiler-toggle-results ()
  "Toggle compilation results - equivalent to :CompilerToggleResults."
  (interactive)
  (if-let ((comp-buffer (get-buffer "*compilation*")))
      (if (get-buffer-window comp-buffer)
          (delete-windows-on comp-buffer)
        (display-buffer comp-buffer))
    (message "No compilation results available")))

;;;###autoload
(defun compiler-stop ()
  "Stop all tasks - equivalent to :CompilerStop."
  (interactive)
  (when (and (boundp 'compilation-in-progress) compilation-in-progress)
    (kill-compilation)
    (message "Compilation stopped")))

;;; === SETUP AND KEYBINDINGS ===

;;;###autoload
(defun compiler-clean-output-now ()
  "Immediately clean the current compilation output."
  (interactive)
  (let ((comp-buffers (list "*- C++ compiler*" "*- C compiler*" "*- Python interpreter*" 
                           "*- Go compiler*" "*- Java compiler*" "*- Node.js interpreter*" 
                           "*- Rust compiler*" "*- Rust compiler (Cargo)*")))
    (dolist (buffer-name comp-buffers)
      (when-let ((comp-buffer (get-buffer buffer-name)))
        (with-current-buffer comp-buffer
          (let ((inhibit-read-only t)
                (content (buffer-string)))
            ;; Extract only the actual program output or error messages
            (erase-buffer)
            ;; Look for the actual output after the command
            (cond
             ;; For build and run commands with parentheses
             ((string-match "(rm -f.*?).*?\n\\(\\(?:.\\|\n\\)*\\)\\(?:Compilation finished\\|$\\)" content)
              (insert (string-trim (match-string 1 content))))
             ;; For simple run commands
             ((string-match "\\(python3\\|node\\|java\\|cargo\\).*?\n\\(\\(?:.\\|\n\\)*\\)\\(?:Compilation finished\\|$\\)" content)
              (insert (string-trim (match-string 2 content))))
             ;; For compilation errors
             ((string-match "\\(.*error:.*\\|.*warning:.*\\)" content)
              (let ((lines (split-string content "\n")))
                (dolist (line lines)
                  (when (string-match-p "\\(error:\\|warning:\\|generated\\|failed\\)" line)
                    (insert line "\n")))))
             ;; Default: try to extract meaningful content
             (t
              (let ((lines (split-string content "\n")))
                (dolist (line lines)
                  (unless (string-match-p "\\(^-\\*-\\|^Compilation\\|^(rm -f\\|^mkdir\\|^$\\)" line)
                    (insert line "\n"))))))
            (goto-char (point-min))))))
    (message "✨ All compilation outputs cleaned!")))

;;;###autoload
(defun compiler-setup ()
  "Setup compiler with default keybindings like compiler.nvim."
  (interactive)
  ;; Default keybindings (exact copy of compiler.nvim)
  (global-set-key (kbd "<f6>") #'compiler-open)           ; CompilerOpen
  (global-set-key (kbd "S-<f6>") #'compiler-redo)         ; CompilerRedo
  (global-set-key (kbd "S-<f7>") #'compiler-toggle-results) ; CompilerToggleResults
  
  ;; Additional bindings
  (global-set-key (kbd "C-c c o") #'compiler-open)
  (global-set-key (kbd "C-c c r") #'compiler-redo)
  (global-set-key (kbd "C-c c t") #'compiler-toggle-results)
  (global-set-key (kbd "C-c c s") #'compiler-stop)
  (global-set-key (kbd "C-c c c") #'compiler-clean-output-now)  ; Manual clean output
  
  (message "Compiler setup complete. Press F6 to open compiler."))

;; Auto-setup on load
(compiler-setup)

;; 添加到你的 compiler.el 文件中或单独的配置文件

;;;###autoload
(defun compiler-quick-build-and-run ()
  "Quick build and run - automatically selects build and run option for current filetype."
  (interactive)
  (let ((filetype (compiler--detect-filetype))
        (bau-list (compiler--detect-bau)))
    (cond
     ;; 优先检测构建工具
     ((member 'cargo bau-list)
      (compiler--run-bau-action 'cargo "run"))
     
     ((member 'npm bau-list)
      (compiler--run-bau-action 'npm "start"))
     
     ((member 'make bau-list)
      (compiler--run-bau-action 'make "all"))
     
     ((member 'maven bau-list)
      (compiler--run-bau-action 'maven "compile"))
     
     ((member 'gradle bau-list)
      (compiler--run-bau-action 'gradle "run"))
     
     ;; 根据文件类型选择
     ((eq filetype 'cpp)
      (compiler-cpp-action "option1"))
     
     ((eq filetype 'c)
      (compiler-c-action "option1"))
     
     ((eq filetype 'python)
      (compiler-python-action "option1")) ; Run this file
     
     ((eq filetype 'go)
      (compiler-go-action "option1"))
     
     ((eq filetype 'java)
      (compiler-java-action "option1"))
     
     ((eq filetype 'javascript)
      (compiler-javascript-action "option1")) ; Run this file
     
     ((eq filetype 'rust)
      (compiler-rust-action "option1"))
     
     ;; 默认情况
     (t
      (message "❌ No supported filetype detected or no build configuration found")))))

;;;###autoload
(defun compiler-smart-run ()
  "Smart compiler run: first run builds and runs, subsequent calls redo the last action."
  (interactive)
  (if compiler--last-selected
      ;; 如果有上次的编译记录，直接重做
      (progn
        (message "🔄 Redoing last compilation...")
        (compiler-redo))
    ;; 否则执行快速构建和运行
    (progn
      (message "🚀 Quick build and run...")
      (compiler-quick-build-and-run))))

;; Evil mode 键绑定设置
;;;###autoload
(defun compiler-setup-evil-keybindings ()
  "Setup Evil mode keybindings for compiler."
  (interactive)
  (when (fboundp 'evil-define-key)
    ;; 在 normal state 绑定 SPC-r-r 为智能运行
    (evil-define-key 'normal 'global (kbd "SPC r r") 'compiler-smart-run)
    
    ;; 额外的便捷键绑定
    (evil-define-key 'normal 'global (kbd "SPC r o") 'compiler-open)       ; 打开编译器选择
    (evil-define-key 'normal 'global (kbd "SPC r R") 'compiler-redo)       ; 强制重做
    (evil-define-key 'normal 'global (kbd "SPC r t") 'compiler-toggle-results) ; 切换结果
    (evil-define-key 'normal 'global (kbd "SPC r s") 'compiler-stop)       ; 停止编译
    (evil-define-key 'normal 'global (kbd "SPC r c") 'compiler-clean-output-now) ; 清理输出
    
    ;; 更快的单键绑定（可选）
    (evil-define-key 'normal 'global (kbd "SPC c c") 'compiler-smart-run)       ; 快速运行
    (evil-define-key 'normal 'global (kbd "SPC c r") 'compiler-redo)           ; 重做
    
    (message "✅ Evil compiler keybindings setup complete!")
    (message "💡 Use 'SPC r r' or 'gR' to smart run, 'SPC r R' or 'g R' to redo")))

;; 自动设置（如果检测到 evil-mode）
(eval-after-load 'evil
  '(compiler-setup-evil-keybindings))

;; 手动设置函数（如果需要）
;;;###autoload  
(defun compiler-setup-with-evil ()
  "Setup compiler with Evil mode integration."
  (interactive)
  (compiler-setup)  ; 原有的设置
  (compiler-setup-evil-keybindings)  ; Evil 键绑定
  (message "🎯 Compiler with Evil mode setup complete!"))

;; 使用说明注释：
;;
;; 使用方法：
;; 1. 在你的 init.el 中添加: (compiler-setup-with-evil)
;; 2. 或者手动调用: M-x compiler-setup-evil-keybindings
;;
;; 键绑定说明：
;; - SPC r r / gR    : 智能运行（首次构建运行，之后重做）
;; - SPC r o         : 打开编译器选择菜单
;; - SPC r R / g R   : 强制重做上次编译
;; - SPC r t         : 切换编译结果显示
;; - SPC r s         : 停止当前编译
;; - SPC r c         : 清理输出
;;
;; 智能运行逻辑：
;; 1. 首次按 SPC r r：自动检测文件类型和构建工具，执行 build and run
;; 2. 再次按 SPC r r：直接重做上次的编译操作
;; 3. 构建工具优先级：Cargo > NPM > Make > Maven > Gradle > 文件类型检测

(provide 'compiler)

