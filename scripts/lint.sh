#!/usr/bin/env bash

EMACS="${EMACS:=emacs}"

# Get the directory of the git root
root_dir=$(git rev-parse --show-toplevel)

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
  ERROR_ON_WARN=nil
else
  ERROR_ON_WARN=t
fi

NEEDED_PACKAGES="cl-lib let-alist compat package-lint"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Function to remove all .elc files in the project
remove_elc_files() {
  find "${root_dir}" -name '*.elc' -type f -delete
}

lint_file() {
  "$EMACS" -Q -batch \
           --eval "$INIT_PACKAGE_EL" \
           -f package-lint-batch-and-exit \
           $1
}

lint_files() {
  # Get the staged files
  el_files=$(find "${root_dir}" -type f -name '*.el')

  # Process each file
  for file in $el_files
  do
    full_path="${root_dir}/${file}"

    if [[ "$(basename "$file")" == ".dir-locals.el" ]]; then
      continue
    fi

    # Run Emacs linting in batch mode with an absolute path to elisp-lint.el
    lint_file $file
    # Capture the exit code
    exit_code=$?

    # Check if linting was successful
    if [ $exit_code -ne 0 ]; then
      exit $exit_code
    fi
  done
}

remove_elc_files
lint_files
remove_elc_files
