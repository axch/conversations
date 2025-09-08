────────────────────────────────────────────────────────────────────────────
Usage
────────────────────────────────────────────────────────────────────────────
• Author a post at: posts/2017-11-19-compositional-statistical-unit-testing.html.pmd
  with front‑matter like:

  ---
  title: Compositional statistical unit testing
  date: 2017/11/19
  author: Alexey Radul
  # published: false   # optional; include in Preview, exclude in Production
  ---
  Body in Pandoc Markdown…

  If you add a sibling directory posts/2017-11-19-compositional-statistical-unit-testing/
  (images, attachments), it will be copied to ideas/2017/compositional-statistical-unit-testing/…

• Dev loop (Preview; includes unpublished):
    cabal run site -- watch
  Browse http://127.0.0.1:8000/  (hit refresh after edits)

• Production build (excludes unpublished):
    cabal run site -- build
  Output goes to _site/ ready to publish (e.g., GitHub Pages).

• Pin versions fully:
    cabal update && cabal freeze
  This writes cabal.project.freeze with exact versions for full reproducibility.

