#!/bin/bash

rm -rf \
   elpa \
   tmp \
   auto-save-list \
   projectile-bookmarks.eld \
   projectile.cache \
   irony \
   modules/*.elc \
   ac-comphist.dat \
   *~ \
   bookmarks \
   .python-environments/ \
   eln-cache \
   .lsp-session-v1 \
   history \
   places \
   recentf \
   transient \
   .dap-breakpoints \
   tree-sitter

# cleanup straight
rm -rf straight/build-cache.el straight/build
