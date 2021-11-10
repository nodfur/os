inputs: self: super:

{
  mu4e-dashboard =
    self.emacsPackages.trivialBuild {
      pname = "mu4e-dashboard";
      version = "0.0.0";
      src = inputs.mu4e-dashboard;

      preBuild = ''
        export EMACSLOADPATH=${self.mu}/share/emacs/site-lisp/mu4e:$EMACSLOADPATH
      '';
    };

  mu4e-thread-folding =
    self.emacsPackages.trivialBuild {
      pname = "mu4e-thread-folding";
      version = "0.0.0";
      src = inputs.mu4e-thread-folding;

      preBuild = ''
        export EMACSLOADPATH=${self.mu}/share/emacs/site-lisp/mu4e:$EMACSLOADPATH
      '';
    };

  nano-emacs =
    self.emacsPackages.trivialBuild {
      pname = "nano-emacs";
      version = "0.0.0";
      src = inputs.nano-emacs;

      packageRequires = with self.emacsPackages; [
        ts smex svg-tag-mode mini-frame

        self.mu4e-dashboard self.mu4e-thread-folding
      ];

      preBuild = ''
        export HOME=$TMPDIR
        export EMACSLOADPATH=${self.mu}/share/emacs/site-lisp/mu4e:$EMACSLOADPATH
      '';
    };
}
