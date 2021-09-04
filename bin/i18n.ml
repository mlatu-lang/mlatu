open! Batteries

include
  Gettext.Program (struct
      let textdomain = "mlatu"
      let codeset = None
      let dir = None
      let dependencies = Mlatu_api.I18n.init
    end)
    (GettextCamomile.Map)
