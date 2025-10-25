(= (change-state! $var $value)
   (let $temp (collapse (match &self (state $var $prevvalue) 
                                     (remove-atom &self (state $var $prevvalue))))
        (add-atom &self (state $var $value))))

(= (bind! $var (new-state $value))
   (change-state! $var $value))

(= (get-state $state)
   (once (match &self (state $state $value) $value)))

!(bind! state (new-state rest))
!(test (get-state state) rest)
!(change-state! state active)
!(test (get-state state) active)
