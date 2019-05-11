(declare (uses configuration))

(read-configuration-file "example.cfg"
  (lambda (configuration-node)

    (display "Fire Power: ")
    (display
      (configuration-section-get-value
        configuration-node "fire-power"))
    (newline)

    (display "Evil Plan's Third Secret Number: ")
    (display
      (configuration-list-get-value
        (configuration-section-get-list
          (configuration-section-get-section
            configuration-node "evil-plan")
          "secret-numbers")
        2))
    (newline)))
