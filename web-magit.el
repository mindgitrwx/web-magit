(defun get-github-repo-url ()
  "Get the URL of the current GitHub repository in the current buffer."
  (interactive)
  (let* ((git-url (shell-command-to-string "git remote -v | grep origin | awk '{print $2}' | head -n1"))
         (git-url (string-trim git-url))
         (message (format "git-url: %s" git-url))
         (web-url (replace-regexp-in-string
                   "\\.git$" "" (replace-regexp-in-string
                                 "^git@github.com:" "https://github.com/" git-url)))
         (message (format "web-url: %s" web-url)))
    web-url))

(defun open-github-repo ()
  "Open the current Github repository on browser"
  (interactive)
  (browse-url (get-github-repo-url)))

(defun open-github-repo-issues ()
  "Open the current Github issue on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/issues")))

(defun open-github-repo-issues-by-username (username)
  "Open the current Github issue by username on browser"
  (interactive "sEnter your GitHub username: ")
  (browse-url (concat (my-get-github-repo-url) "/issues?q=is%3Aissue+author%3A" username)))

(defun open-github-repo-pull-requests ()
  "Open the current Github pull-requests on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/pulls")))

(defun open-github-repo-actions ()
  "Open the current Github actions on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/actions")))

(defun open-github-repo-file ()
  "Open the current Github files on browser"
  (interactive)
  (browse-url (concat (get-github-repo-url) "/blob/master/" (file-name-nondirectory(buffer-file-name)))))

(defun open-issue-template ()
  "Open the issue-template directory; open file if only one exists, else open directory."
  (interactive)
  (let* ((root (vc-root-dir))
         (issue-template-dir (concat root ".github/ISSUE_TEMPLATE/")))
    (if (eq 1 (length (directory-files issue-template-dir)))
        (find-file (concat issue-template-dir (car (directory-files issue-template-dir))))
      (dired issue-template-dir)))))

(defun open-setting-github-token ()
  "Browse url to setting token"
  (interactive)
  (browse-url "https://github.com/settings/tokens"))


(defun create-github-repo (repo-name &optional username token)
  "Create a new repository on GitHub with the given REPO-NAME.

Optionally, specify a USERNAME and TOKEN to use for authentication. If not
provided, the user will be prompted to enter them."
  (interactive "sRepository name: ")
  (unless username
    (setq username (read-string "GitHub username: ")))
  (unless token
    (setq token (read-passwd "GitHub personal access token: ")))
  (let ((url (format "https://api.github.com/user/repos -u %s:%s" username token)))
    (call-process-shell-command (format "curl -d '{\"name\":\"%s\"}' %s" repo-name url)))

  (sleep-for 1)
  (call-process-shell-command (format "mkdir %s" repo-name))
  (call-process-shell-command (format "cd %s" repo-name))
  (call-process-shell-command (format "echo \"# %s\" >> README.md" repo-name))
  (call-process-shell-command "git init")
  (call-process-shell-command "git add README.md")
  (call-process-shell-command (format "git commit -m \"Initial commit\""))
  (call-process-shell-command "git branch -M main")
  (call-process-shell-command (format "git remote add origin https://github.com/%s/%s.git" username repo-name))
  (call-process-shell-command "git push -u origin main"))

(defun open-recently-merged-pull-reqeusts()
  "opne browser and list recently merged pull request from user"
  (interactive)
  (let ((username (read-string "Enter your GitHub username: ")))
    (browse-url (concat "https://github.com/search?q=is%3Apr+is%3Amerged+author%3A" username "+sort%3Aupdated-desc&type=issues"))))
