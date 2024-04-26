import { DefaultUpdater } from 'release-please/build/src/updaters/default';

// Regex to match the version line in an Emacs Lisp file
const ELISP_VERSION_REGEX = /^;;; Version: (.*)$/m;

/**
 * Updates an Emacs Lisp file which is expected to have a version string in the format:
 * ;;; Version: x.x.x
 */
export class ElispVersionUpdater extends DefaultUpdater {
  /**
   * Given initial file contents, return updated contents.
   * @param {string} content The initial content of the file.
   * @returns {string} The updated content with the new version.
   */
  updateContent(content: string): string {
    return content.replace(
      ELISP_VERSION_REGEX,
      `;;; Version: ${this.version}`
    );
  }
}
