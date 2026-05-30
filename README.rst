Personal Emacs Configuration
----------------------------

Stuff I've gathered over the many years of using emacs. Latest update to Debian 13.

A lot of the 3rd party scripts are directly copied in order to save users from hunting this across the net.

Assuming thiis cloned to ~/rdiankov-emacs-config, execute the following to setup in system:

Debian:
::
   
   sudo apt-get install install-info xfonts-75dpi elpa-bm elpa-avy elpa-go-mode elpa-auto-complete elpa-js2-mode elpa-vterm elpa-projectile elpa-lsp-mode elpa-flycheck

All:

.. code-block::

  git clone https://github.com/rdiankov/emacs-config.git $HOME/rdiankov-emacs-config
  ln -f -s $HOME/rdiankov-emacs-config/.emacs-lisp $HOME/.emacs-lisp
  ln -f -s $HOME/rdiankov-emacs-config/.emacs $HOME/.emacs

Can byte-compile everything in the .emacs-lisp directory to accelerate the startup by running::

  C-u 0 M-x byte-recompile-directory

  
Warning: This repository holds the emacs configuration I used everyday for my current computer systems (Debian these days).
