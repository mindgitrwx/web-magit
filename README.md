# README
This file contains a set of functions that allow you to interact with a GitHub repository from within an Emacs buffer.

# Function Descriptions
## my-get-github-repo-url
This function retrieves the URL of the GitHub repository for the current buffer. It does so by using the vc-git-root and vc-git--run-command-string functions to determine the root directory of the Git repository and then running the git config command to retrieve the URL of the origin remote. The URL is then cleaned up by removing the trailing newline and, if present, the .git suffix.

## my-open-github-repo
This function opens the GitHub repository for the current buffer in the default web browser. It does so by calling the browse-url function with the URL of the repository, which is obtained by calling the my-get-github-repo-url function.

## my-open-github-repo-issues
This function opens the "Issues" page of the GitHub repository for the current buffer in the default web browser. It does so by calling the browse-url function with the URL of the "Issues" page, which is obtained by calling the my-get-github-repo-url function and appending /issues to the result.

## my-open-github-repo-pulls
This function opens the "Pull Requests" page of the GitHub repository for the current buffer in the default web browser. It does so by calling the browse-url function with the URL of the "Pull Requests" page, which is obtained by calling the my-get-github-repo-url function and appending /pulls to the result.

## my-open-github-repo-actions
This function opens the "Actions" page of the GitHub repository for the current buffer in the default web browser. It does so by calling the browse-url function with the URL of the "Actions" page, which is obtained by calling the my-get-github-repo-url function and appending /actions to the result.

## my-open-github-repo-file
This function opens the current file on the "master" branch of the GitHub repository for the current buffer in the default web browser. It does so by calling the browse-url function with the URL of the file, which is obtained by calling the my-get-github-repo-url function, appending /blob/master/, and then appending the name of the current file. The name of the current file is obtained using the buffer-file-name and file-name-nondirectory functions
