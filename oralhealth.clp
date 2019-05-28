/*
* Russell Yang
* 5/12/2019
* Diagnoses common oral & dental pathologies using backward chaining.
* Expert: Dr. William Tong from Dental Solutions
*/

; Remove all rules, etc.
(clear)
; Remove all facts from working memory
(reset)

; Define a global variable ?*q* to be 1. This will be used to count the question number
(defglobal ?*q* = 1)

/*
* The following lines of code mark each characteristic as being eligible for
* backward chaining. The characteristics are explained on the inline comments.
*/
(do-backward-chaining inflamedgums)     ; Whether the person's gums are inflamed
(do-backward-chaining regbrushfloss)    ; Whether the person regularly brushes and flosses
(do-backward-chaining bleedinggums)     ; Whether the person's gums easily bleed
(do-backward-chaining bonelevelchange)  ; Whether the person has had a recent bone level change
(do-backward-chaining ongums)           ; Whether the problem is on the gums
(do-backward-chaining onlips)           ; Whether the problem is on the lips
(do-backward-chaining ontongue)         ; Whether the problem is on the tongue
(do-backward-chaining ontooth)          ; Whether the problem is on the tooth
(do-backward-chaining inmouth)          ; Whether the problem is inside the mouth
(do-backward-chaining sores)            ; Whether the person has sores
(do-backward-chaining badbreath)        ; Whether the person has bad breath
(do-backward-chaining hardtobreathe)    ; Whether it's hard for the person to breathe
(do-backward-chaining appetite)         ; Whether the person's appetite has recently changed
(do-backward-chaining mouthinjury)      ; Whether the person has had recent mouth injury/trauma
(do-backward-chaining tendertongue)     ; Whether the person's tongue is tender
(do-backward-chaining normalapptongue)  ; Whether the person's tongue appears normal
(do-backward-chaining painbiting)       ; Whether the person feels pain when biting down
(do-backward-chaining painhot)          ; Whether the person feels pain when eating/drinking something hot
(do-backward-chaining paincold)         ; Whether the person feels pain when eating/drinking something cold
(do-backward-chaining prevcavities)     ; Whether the person has previously had cavities
(do-backward-chaining teethpainful)     ; Whether the person has painful teeth
(do-backward-chaining teethloose)       ; Whether the person's has loose teeth
(do-backward-chaining teethcolorchange) ; Whether the person's tooth color has recently changed
(do-backward-chaining puffysores)       ; Whether the person's sores are puffy

/*
* This function prints the instructions to the user, and then runs the file
* to start the game.
*/
(deffunction oralhealth ()
   ; Tell the user what the expert system does
   (printout t "This expert system diagnoses common oral health conditions." crlf)
   (printout t "You will be asked a series of questions." crlf crlf)
   ; Tell the user that anything starting with y/Y counts as yes.
   (printout t "Any word that starts with y or Y is interpreted as yes." crlf)
   ; Tell the user than anything starting with n/N counts as no.
   (printout t "Any word that starts with n or N is interpreted as no." crlf)
   ; Tell the user that anything not starting with y/Y/n/N will be rejected
   (printout t "Any word that does not start with y/Y/n/N will be rejected." crlf)
   ; Tell the user that they will be re-prompted until a valid input is given
   (printout t "You will be re-prompted until you enter a valid input." crlf crlf)
   ; Tell the user to hit enter after they enter then answers.
   (printout t "Hit enter after typing your response." crlf)

   ; Start the program!
   (run)
   ; All functions end with a return.
   (return)
)

/*
* This function is a very basic function to ask the user a question, and read
* back their input. 
* Parameters:
* ?question - the question to be asked to the user
*/
(deffunction ask (?question)
   (printout t ?question) ; Printout the given question
   (return (read))        ; Return what the user inputs
)

/*
* This function, which uses the ask function as a helper
* function, will keep prompting a user with a given ?question until they answer
* something that can be interpreted as either yes or no. The function also
* prints out the question number before each question, and increments the 
* question number on each call.
*
* The helper function
* check is used to determine if the user's response to the question starts
* with y/Y or n/N. If the user's response starts with y/Y, then the check function
* will convert that to yes. If the user's response starts with n/N, then the check
* function will convert that to no. If the user's response is not a symbol (ex: 5),
* the check function will convert that to nil. This makes it easy for the 
* validatedAsk function to understand the user's response and tell if it is valid.
*
* While the user's response is not valid, the user is prompted to re-enter
* a response.
*
* At the end of the function, the ?result variable, which is guaranteed to be
* either yes or no, is returned.
*
* Parameters:
* ?question - the question to be asked to the user (ex: "Is it a mammal?")
*/
(deffunction validatedAsk (?question)
   (printout t ?*q*) ; Printout the question number
   (printout t ". ") ; Printout a period and a space

   /*
   * Use the ask function to ask the
   * question ?question, and bind that to the ?answer variable.
   */
   (bind ?answer (ask ?question))

   /*
   * Pass ?answer to the check function, so that it can be converted to
   * either yes, no, or nil. Bind the output to ?result.
   */
   (bind ?result (check ?answer))
   ; While the result is equal to nil (which means it isn't yes or no)
   (while (= ?result nil) do
      ; Tell the user that the input was not valid.
      (printout t "Input not valid. Please try again." crlf)
      ; Printout question number
      (printout t ?*q*)
      (printout t ". ")
      ; Ask the user again
      (bind ?answer (ask ?question))
      ; Pass ?answer to check function again
      (bind ?result (check ?answer))
   )

   (bind ?*q* (+ 1 ?*q*)) ; Increment the question number ?*q* by 1

   ; At this point, ?result is guaranteed to be yes or no. Return it.
   (return ?result)
)

/*
* The check function takes an input, and returns yes if the input starts with
* y or Y, returns no if the input starts with n or N, and returns nil otherwise.
* Returning nil could mean that the input was not a symbol (ex: 5), or that
* it didn't start with y/Y/n/N (ex: go).
*
* Parameters:
* ?in - an user input to be processed
*/
(deffunction check (?in)
   ; If ?in is a symbol
   (if (symbolp ?in) then
      /*
      * Use the sub-string method to find if the first character in the string
      * is y or Y. If so, change ?in to yes.
      */
      (if (or (= "y" (sub-string 1 1 ?in)) (= "Y" (sub-string 1 1 ?in))) then
         (bind ?in yes)
      /*
      * Use the sub-string method to find if the first character in the string
      * is n or N. If so, change ?in to no.
      */
      elif (or (= "n" (sub-string 1 1 ?in)) (= "N" (sub-string 1 1 ?in))) then
         (bind ?in no)
      else
         (bind ?in nil)
      )
   ; Otherwise, the input was not valid, so bind nil to ?in.
   else
      (bind ?in nil)
   )
   ; Return ?in
   (return ?in)
)

/*
* The noneFound rule is intended to fire if no oral conditions can be found that
* are consistent with what the user inputs. Therefore, the rule is given
* a low salience, or priority. The pattern to be matched is (not (done))
* since when a match is found, done is asserted. Thus, if
* at the end, the pattern (not (done)) is found, no match has been found.
*/
(defrule noneFound "Finish up if no conditions can be found"
   ; Declare a relatively low value for salience so the rule only might fire at the end
   (declare (salience -100))
   ; This is the pattern we are looking for.
   (not (done))
   =>
   ; Tell the user that no conditions matched.
   (printout t "Couldn't find any oral conditions that matched." crlf)
)

/*
* This function prints the user's diagnosis and asserts (done) so that the noneFound
* rule will not fire. It then halts so that no more expert systems rules fire, and
* it includes a return statement at the very end.
*
* Parameters:
* ?condition - an oral condition that the system thinks the user has
*/
(deffunction end (?condition)
   (printout t "You have ") ; Print out "You may have " 
   (printout t ?condition)  ; Print out the user's condition
   (printout t "." crlf)    ; Print out a period
   (assert (done))          ; Assert the pattern (done)
   (halt)                   ; Stop the expert system rules from firing
   (return)                 ; All functions end with a return
)

/*
* This rule will fire if the characteristics of gingivitis 
* are met. The LHS contains the characteristics
* of gingivitis, and the RHS prints out calls the end function,
* passing the condition gingivitis (early stage gum disease)
*/
(defrule gingivitis "Matches for gingivitis"
   (inflamedgums yes)
   (regbrushfloss no)
   (bleedinggums yes)
   (bonelevelchange no)
   (sores no)
   (badbreath yes)
   =>
   (end "gingivitis")
)

/*
* This rule will fire if the characteristics of periodontitis 
* are met. The LHS contains the characteristics
* of periodontitis, and the RHS prints out calls the end function,
* passing the condition periodontitis (late stage gum disease)
*/
(defrule periodontitis "Matches for periodontitis"
   (inflamedgums yes)
   (regbrushfloss no)
   (bleedinggums yes)
   (bonelevelchange yes) ; Unlike gingivitis, periodontitis is characterized by a bone level change
   (sores no)
   (badbreath yes)
   =>
   (end "periodontitis")
)

/*
* This rule will fire if the characteristics of tongue cancer 
* are met. The LHS contains the characteristics
* of tongue cancer, and the RHS prints out calls the end function,
* passing the condition tongue cancer.
*/
(defrule tonguecancer "Matches for tongue cancer"
   (ontongue yes)
   (sores yes)
   (inflamedgums no)
   (bleedinggums no)
   (appetite yes)
   (tendertongue yes)
   (badbreath no) ; Surprisingly, oral cancers such as tongue cancer don't usually cause bad breath
   (paincold yes)
   (painhot yes)
   (teethloose no)
   =>
   (end "tongue cancer")
)

/*
* This rule will fire if the characteristics of canker sores 
* are met. The LHS contains the characteristics
* of canker sores, and the RHS prints out calls the end function,
* passing the condition canker sores.
*/
(defrule cankersores "Matches for canker sores"
   (sores yes)
   (onlips yes)
   (appetite no)
   (inflamedgums no)
   (puffysores yes) ; Canker sores are puffy, whereas cold sores are not
   =>
   (end "canker sores")
)

/*
* This rule will fire if the characteristics of cold sores 
* are met. The LHS contains the characteristics
* of cold sores, and the RHS prints out calls the end function,
* passing the condition cold sores.
*/
(defrule coldsores "Matches for cold sores"
   (sores yes)
   (onlips yes)
   (appetite no)
   (inflamedgums no)
   (puffysores no)
   =>
   (end "cold sores")
)

/*
* This rule will fire if the characteristics of burning mouth syndrome 
* are met. The LHS contains the characteristics
* of burning mouth syndrome, and the RHS prints out calls the end function,
* passing the condition burning mouth syndrome.
*/
(defrule burningmouthsyndrome "Matches for burning mouth syndrome"
   (ontongue yes)
   (tendertongue yes)
   (normalapptongue yes)
   (inmouth yes)
   (mouthinjury yes)
   (inflamedgums no)
   (appetite yes)
   (badbreath no)
   =>
   (end "burning mouth syndrome")
)

/*
* This rule will fire if the characteristics of tonsillitis 
* are met. The LHS contains the characteristics
* of tonsillitis, and the RHS prints out calls the end function,
* passing the condition tonsillitis.
*/
(defrule tonsillitis "Matches for tonsillitis"
   (badbreath yes)
   (hardtobreathe yes)
   (regbrushfloss no)
   (inflamedgums no)
   (sores no)
   (appetite yes)
   (painbiting no)
   (painhot no)
   (paincold no)
   =>
   (end "tonsillitis")
)

/*
* This rule will fire if the characteristics of cavities 
* are met. The LHS contains the characteristics
* of cavities, and the RHS prints out calls the end function,
* passing the condition cavities.
*/
(defrule cavities "Matches for cavities"
   (ontooth yes)
   (painbiting yes)
   (painhot yes)
   (paincold yes)
   (inflamedgums no)
   (regbrushfloss no)
   (sores no)
   (hardtobreathe no)
   (teethcolorchange yes)
   (teethpainful yes)
   =>
   (end "cavities")
)

/*
* This rule will fire if the characteristics of tooth abscess 
* are met. The LHS contains the characteristics
* of tooth abscess, and the RHS prints out calls the end function,
* passing the condition tooth abscess.
*/
(defrule toothabscess "Matches for tooth abscess"
   (prevcavities yes)
   (ontooth yes)
   (badbreath yes)
   (hardtobreathe no)
   (teethloose yes)
   (inflamedgums yes)
   (bleedinggums yes)
   (sores no)
   (teethpainful yes)
   =>
   (end "tooth abscess")
)

/*
* This rule will fire if the characteristics of tooth erosion 
* are met. The LHS contains the characteristics
* of tooth erosion, and the RHS prints out calls the end function,
* passing the condition tooth erosion.
*/
(defrule tootherosion "Matches for tooth erosion"
   (painhot yes)
   (paincold yes)
   (painbiting no)
   (ontooth yes)
   (onlips no)
   (inflamedgums no)
   (regbrushfloss no)
   (sores no)
   (badbreath no)
   (hardtobreathe no)
   =>
   (end "tooth erosion")
)

/*
* This rule will assert yes/no for the characteristic inflamedgums (whether the person has inflamed gums). 
* It will also assert yes/no for the characteristics that must logically follow
* if the person does not have inflamed gums (gums can't be bleeding).
*
* If the value of inflamedgums is needed, then the validatedAsk function
* is called, passing in "Are your gums inflamed? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is no (ie, the person doesn't have inflamed gums, we also know for sure
* that the person's gums aren't bleeding, since gums have to be inflamed to bleed. Thus,
* that known piece of information is asserted as well.
* Finally, (inflamedgums ?a) is asserted.
*/
(defrule need-inflamedgums-rule "Rule to backward chain the characteristic inflamedgums"
   (need-inflamedgums ?) ; The LHS is if inflamedgums is needed
   =>
   ; Call validatedAsk to ask if the person has inflamed gums, save to ?a
   (bind ?a (validatedAsk "Are your gums inflamed? "))
   ; If ?a is no, then we also know some more info
   (if (= ?a no) then
      (assert (bleedinggums no)) ; If the gums are not inflamed, they cannot be bleeding
   )
   (assert (inflamedgums ?a)) ; assert inflamedgums with its value ?a
)

/*
* This rule will assert yes/no for the characteristic regbrushfloss
* (whether the person regularly brushes and flosses).
*
* If the value of regbrushfloss is needed, then the validatedAsk function is called, passing
* in "Do you regularly brush and floss? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with regbrushfloss.
*/
(defrule need-regbrushfloss-rule "Rule to backward chain the characteristic regbrushfloss"
   (need-regbrushfloss ?) ; The LHS is if regbrushfloss is needed
   =>
   ; Call validatedAsk to ask if the person regularly brushes and flosses, assert regbrushfloss with the output
   (assert (regbrushfloss (validatedAsk "Do you regularly brush and floss? ")))
)

/*
* This rule will assert yes/no for the characteristic bleedinggums (whether the person's gums easily bleed). 
* It will also assert yes/no for the characteristics that must logically follow
* if the person does have gums that easily bleed (gums must be inflamed).
*
* If the value of bleedinggums is needed, then the validatedAsk function
* is called, passing in "Do your gums bleed easily? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the person has gums that easily bleed, we also know for sure
* that the person's gums are inflamed, since gums have to be inflamed to bleed. Thus,
* that known piece of information is asserted as well.
* Finally, (bleedinggums ?a) is asserted.
*/
(defrule need-bleedinggums-rule "Rule to backward chain the characteristic bleedinggums"
   (need-bleedinggums ?) ; The LHS is if bleedinggums is needed
   =>
   ; Call validatedAsk to ask if the person has bleeding gums, save to ?a
   (bind ?a (validatedAsk "Do your gums bleed easily? "))
   ; If ?a is yes, then we also know some more info
   (if (= ?a yes) then
      (assert (inflamedgums yes)) ; All bleeding gums are also inflamed
   )
   (assert (bleedinggums ?a)) ; assert bleedingugms with its value ?a
)

/*
* This rule will assert yes/no for the characteristic bonelevelchange
* (whether the person's bone level has changed recently).
*
* If the value of bonelevelchange is needed, then the validatedAsk function is called, passing
* in "Has your bone level changed recently (reduction in spongy tissue)? " as the ?question. 
* The output of validatedAsk (yes/no), is asserted with bonelevelchange.
*/
(defrule need-bonelevelchange-rule "Rule to backward chain the characteristic bonelevelchange"
   (need-bonelevelchange ?) ; The LHS is if bonelevelchange is needed
   =>
   ; Call validatedAsk to ask if the person's bone level has changed recently, assert bonelevelchange with the output
   (assert (bonelevelchange (validatedAsk "Has your bone level changed recently (reduction in spongy bone tissue)? ")))
)

/*
* This rule will assert yes/no for the characteristic ongums (whether the primary affected
* place is the gums). It will also assert yes/no for the characteristics that must logically 
* follow if the primary affected place is the gums.
*
* If the value of ongums is needed, then the validatedAsk function
* is called, passing in "Are your gums the place that is primarily affected? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the gums are the place that is primarily affected, we also know for sure
* that the primary affected place cannot be the lips, tongue, or teeth. Also, we know for sure
* that the problem is related to something inside the mouth, since the gums are inside
* the mouth. Thus, those known pieces of information is asserted as well.
* Finally, (ongums ?a) is asserted.
*/
(defrule need-ongums-rule "Rule to backward chain the characteristic ongums"
   (need-ongums ?) ; The LHS is if ongums is needed
   =>
   ; Call validatedAsk to ask if the person's gums are the primary place that is affected, save to ?a
   (bind ?a (validatedAsk "Are your gums the place that is primarily affected? "))
   ; If the gums are the primary affected place, then we also know some more info
   (if (= ?a yes) then
      (assert (onlips no))   ; Lips can't be the primary affected place
      (assert (ontongue no)) ; Tongue can't be the primary affected place
      (assert (ontooth no))  ; Tooth (teeth) can't be the primary affected place
      (assert (inmouth yes)) ; Must be in the mouth
   )
   (assert (ongums ?a)) ; assert ongums with its value ?a
)

/*
* This rule will assert yes/no for the characteristic onlips (whether the primary affected
* place is the lips). It will also assert yes/no for the characteristics that must logically 
* follow if the primary affected place is the lips.
*
* If the value of onlips is needed, then the validatedAsk function
* is called, passing in "Are your lips the place that is primarily affected? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the lips are the place that is primarily affected, we also know for sure
* that the primary affected place cannot be the gums, tongue, or teeth. Also, we know for sure
* that the problem isn't related to something inside the mouth, since the lips aren't inside
* the mouth. Additionally, we know for sure that the gums cannot be inflamed and that
* they cannot be bleeding since inflamed gums and bleeding gums would only occur if the 
* primary affected area was the gums. Thus, those known pieces of information is asserted as well.
* Finally, (onlips ?a) is asserted.
*/
(defrule need-onlips-rule "Rule to backward chain the characteristic onlips"
   (need-onlips ?) ; The LHS is if onlips is needed
   =>
   ; Call validatedAsk to ask if the person's lips are the primary place that is affected, save to ?a
   (bind ?a (validatedAsk "Are your lips the place that is primarily affected? "))
   ; If the lips are the primary affected place, then we also know some more info
   (if (= ?a yes) then
      (assert (ongums no))       ; Gums can't be the primary affected place
      (assert (ontongue no))     ; Tongue can't be the primary affected place
      (assert (ontooth no))      ; Tooth (teeth) can't be the primary affected place
      (assert (inmouth no))      ; Must not be in the mouth (lips are NOT in the mouth)
      (assert (inflamedgums no)) ; Gums can't be inflamed since they aren't primarily affected
      (assert (bleedinggums no)) ; Gums can't be bleeding since they aren't primarily affected
   )
   (assert (onlips ?a)) ; assert onlips with its value ?a
)

/*
* This rule will assert yes/no for the characteristic ontongue (whether the primary affected
* place is the tongue). It will also assert yes/no for the characteristics that must logically 
* follow if the primary affected place is the tongue.
*
* If the value of ontongue is needed, then the validatedAsk function
* is called, passing in "Is your tongue the place that is primarily affected? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the tongue is the place that is primarily affected, we also know for sure
* that the primary affected place cannot be the lips, gums, or teeth. Also, we know for sure
* that the problem is related to something inside the mouth, since the tongue is inside
* the mouth. Additionally, we know for sure that the gums cannot be inflamed and that
* they cannot be bleeding since inflamed gums and bleeding gums would only occur if the 
* primary affected area was the gums. Thus, those known pieces of information is asserted as well.
* Finally, (ontongue ?a) is asserted.
*/
(defrule need-ontongue-rule "Rule to backward chain the characteristic ontongue"
   (need-ontongue ?) ; The LHS is if ontongue is needed
   =>
   ; Call validatedAsk to ask if the person's tongue is the primary place that is affected, save to ?a
   (bind ?a (validatedAsk "Is your tongue the place that is primarily affected? "))
   ; If the tongue is the primary affected place, then we also know some more info
   (if (= ?a yes) then
      (assert (onlips no))       ; Lips can't be the primary affected place
      (assert (ongums no))       ; Gums can't be the primary affected place
      (assert (ontooth no))      ; Tooth (teeth) can't be the primary affected place
      (assert (inmouth yes))     ; Must be in the mouth
      (assert (inflamedgums no)) ; Gums can't be inflamed since they aren't primarily affected
      (assert (bleedinggums no)) ; Gums can't be bleeding since they aren't primarily affected
   )
   (assert (ontongue ?a)) ; assert ontongue with its value ?a
)

/*
* This rule will assert yes/no for the characteristic ontooth (whether the primary affected
* place is the teeth). It will also assert yes/no for the characteristics that must logically 
* follow if the primary affected place is the teeth.
*
* If the value of ontooth is needed, then the validatedAsk function
* is called, passing in "Are your teeth the place that is primarily affected? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the teeth are the place that is primarily affected, we also know for sure
* that the primary affected place cannot be the lips, tongue, or gums. Also, we know for sure
* that the problem is related to something inside the mouth, since the teeth are inside
* the mouth. If ?a is no (ie, the teeth aren't the place that is primarily affected,
* then we also know that the teeth can't be loose and that there can't be a tooth color change.
* Thus, those known pieces of information is asserted as well.
* 
* See the inline comment below for more information about why certain characteristics are not
* known for sure if ontooth is yes or no. Particularly, ontooth being yes does NOT necessarily
* mean that the gums aren't inflamed or the gums aren't bleeding (the counterexample is 
* tooth abscess). Furthermore, ontooth being no does NOT necessarily mean that there is
* no bone level change (the counterexample is periodontitis).
* Finally, (ontooth ?a) is asserted.
*/
(defrule need-ontooth-rule "Rule to backward chain the characteristic ontooth"
   (need-ontooth ?) ; The LHS is if ontooth is needed
   =>
   ; Call validatedAsk to ask if the person's teeth are the primary place that is affected, save to ?a
   (bind ?a (validatedAsk "Are your teeth the place that is primarily affected? "))
   ; If the teeth are the primary affected place, then we also know some more info
   (if (= ?a yes) then
      (assert (onlips no))   ; Lips can't be the primary affected place
      (assert (ontongue no)) ; Tongue can't be the primary affected place
      (assert (ongums no))   ; Gums can't be the primary affected place
      (assert (inmouth yes)) ; Must be in the mouth
      /*
      * Note - for the need-ontooth-rule, we don't necessarily know that
      * the gums aren't inflamed or the gums aren't bleeding if the condition
      * is primarily on the tooth since some conditions (such as tooth abscess)
      * primarily occur on the tooth but also cause inflamed and bleeding gums.
      */
   ; If the teeth are not the primary affected place, then we also know some more info
    elif (= ?a no) then
      (assert (teethloose no))       ; Teeth can't be loose
      (assert (teethcolorchange no)) ; Teeth color can't change
      /*
      * Note - even if ontooth is no, meaning that the teeth are not
      * the area that is primarily affected, it's still possible for the
      * bone level to have changed. For example, in periodontitis, the primary
      * affected area is the gums, but the bone level also changes. Therefore,
      * even when ontooth is no, we do not automatically know that bonelevelchange
      * is no as well.
      */
   )
   (assert (ontooth ?a)) ; assert ontooth with its value ?a
)


/*
* This rule will assert yes/no for the characteristic inmouth (whether the problem is related to
* something inside the mouth). It will also assert yes/no for the characteristics that must logically 
* follow if the problem is related to something inside of the mouth.
*
* If the value of inmouth is needed, then the validatedAsk function
* is called, passing in "Is the problem related to something inside of your mouth? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the problem is related to something inside the mouth, we
* also know for sure that the primary affected place cannot be the lips, since they are outside
* of the mouth.
* If ?a is no (ie, the problem is NOT related to something inside the mouth, we also know for sure
* that the primary affected place cannot be the gums, tongue, or teeth, since they are all
* inside of the mouth. Thus, those known pieces of information is asserted as well.
* Finally, (inmouth ?a) is asserted.
*/
(defrule need-inmouth-rule "Rule to backward chain the characteristic inmouth"
   (need-inmouth ?) ; The LHS is if inmouth is needed
   =>
   ; Call validatedAsk to ask if the person's condition is inside the mouth, save to ?a
   (assert (inmouth (validatedAsk "Is the problem related to something inside of your mouth? ")))
   ; If the condition is inside the mouth, then we also know some more info
   (if (= ?a yes) then
      (assert (onlips no)) ; The lips are outside of the mouth
   ; If the condition is not inside the mouth, then we know some more info
    elif (= ?a no) then
      (assert (ongums no))   ; The gums are inside the mouth
      (assert (ontongue no)) ; The tongue is inside the mouth 
      (assert (ontooth no))  ; The teeth are inside the mouth
   )
)

/*
* This rule will assert yes/no for the characteristic sores
* (whether the person has sores).
*
* If the value of sores is needed, then the validatedAsk function is called, passing
* in "Do you have sores? " as the ?question. The output of validatedAsk (yes/no),
* is saved to ?a.
* If ?a is no (ie, the person does not have sores), then we also know that the
* person does not have puffy sores, since having sores is a prerequisite to having
* puffy sores. Thus, that known piece of information is asserted as well.
* Finally, (sores ?a) is asserted as well.
*/
(defrule need-sores-rule "Rule to backward chain the characteristic sores"
   (need-sores ?) ; The LHS is if sores is needed
   =>
   ; Call validatedAsk to ask if the person has sores
   (bind ?a (validatedAsk "Do you have sores? "))
   ; If the person doesn't have sores, then we also know some more info
   (if (= ?a no) then
      (assert (puffysores no)) ; Can't have puffy sores if there are no sores
   )
   (assert (sores ?a))
)

/*
* This rule will assert yes/no for the characteristic badbreath
* (whether the person's has bad breath).
*
* If the value of badbreath is needed, then the validatedAsk function is called, passing
* in "Do you have bad breath? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with badbreath.
*/
(defrule need-badbreath-rule "Rule to backward chain the characteristic badbreath"
   (need-badbreath ?) ; The LHS is if badbreath is needed
   =>
   ; Call validatedAsk to ask if the person has bad breath
   (assert (badbreath (validatedAsk "Do you have bad breath? ")))
)

/*
* This rule will assert yes/no for the characteristic hardtobreathe
* (whether it is difficult for the person to breathe).
*
* If the value of hardtobreathe is needed, then the validatedAsk function is called, passing
* in "Is it hard for you to breathe? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with hardtobreathe.
*/
(defrule need-hardtobreathe-rule "Rule to backward chain the characteristic hardtobreathe"
   (need-hardtobreathe ?) ; The LHS is if hardtobreathe is needed
   =>
   ; Call validatedAsk to ask if it is hard for the person to breathe
   (assert (hardtobreathe (validatedAsk "Is it hard for you to breathe? ")))
)

/*
* This rule will assert yes/no for the characteristic appetite
* (whether the person's appetite has changed recently).
*
* If the value of appetite is needed, then the validatedAsk function is called, passing
* in "Has your appetite changed recently? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with appetite.
*/
(defrule need-appetite-rule "Rule to backward chain the characteristic appetite"
   (need-appetite ?) ; The LHS is if appetite is needed
   =>
   ; Call validatedAsk to ask if the person's appetite has changed recently
   (assert (appetite (validatedAsk "Has your appetite changed recently? ")))
)


/*
* This rule will assert yes/no for the characteristic mouthinjury
* (whether the person's has had a recent mouth injury/mouth trauma).
*
* If the value of mouthinjury is needed, then the validatedAsk function is called, passing
* in "Have you had a recent mouth injury or trauma to the mouth? " as the ?question. 
* The output of validatedAsk (yes/no), is asserted with mouthinjury.
*/
(defrule need-mouthinjury-rule "Rule to backward chain the characteristic mouthinjury"
   (need-mouthinjury ?) ; The LHS is if mouthinjury is needed
   =>
   ; Call validatedAsk to ask if the person has had a recent mouth injury
   (assert (mouthinjury (validatedAsk "Have you had a recent mouth injury or trauma to the mouth? ")))
)

/*
* This rule will assert yes/no for the characteristic tendertongue
* (whether the person's has a tender tongue).
*
* If the value of tendertongue is needed, then the validatedAsk function is called, passing
* in "Is your tongue tender? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with tendertongue.
*/
(defrule need-tendertongue-rule "Rule to backward chain the characteristic tendertongue"
   (need-tendertongue ?) ; The LHS is if tendertongue is needed
   =>
   ; Call validatedAsk to ask if the person has a tender tongue
   (assert (tendertongue (validatedAsk "Is your tongue tender? ")))
)

/*
* This rule will assert yes/no for the characteristic normmalapptongue
* (whether the person's has a tongue that appears normal).
*
* If the value of normalapptongue is needed, then the validatedAsk function is called, passing
* in "Does your tongue appear normal (LOOK normal even if it doesn't feel normal)? " as the ?question.
* The output of validatedAsk (yes/no), is asserted with normalapptongue.
*/
(defrule need-normalapptongue-rule "Rule to backward chain the characteristic normalapptongue"
   (need-normalapptongue ?) ; The LHS is if normalapptongue is needed
   =>
   ; Call validatedAsk to ask if the person's tongue appears normal
   (assert (normalapptongue (validatedAsk "Does your tongue appear normal (LOOK normal even if it doesn't feel normal)? ")))
   /*
   * Note - just because a tongue appears normal doesn't mean it isn't tender. Likewise, just because
   * a tongue doesn't appear normal doesn't mean that it is tender. Therefore, the value of normalapptongue
   * does not affect the value of tendertongue, even though it might seem like it would.
   */
)

/*
* This rule will assert yes/no for the characteristic painbiting
* (whether the person's has pain when biting).
*
* If the value of painbiting is needed, then the validatedAsk function is called, passing
* in "Do you have pain when biting? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with painbiting.
*/
(defrule need-painbiting-rule "Rule to backward chain the characteristic painbiting"
   (need-painbiting ?) ; The LHS is if painbiting is needed
   =>
   ; Call validatedAsk to ask if the person has pain when biting
   (assert (painbiting (validatedAsk "Do you have pain when biting? ")))
)

/*
* This rule will assert yes/no for the characteristic painhot
* (whether the person's has pain when eating/drinkingsomething hot).
*
* If the value of painhot is needed, then the validatedAsk function is called, passing
* in "Do you have pain when eating/drinking something hot? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with painhot.
*/
(defrule need-painhot-rule "Rule to backward chain the characteristic painhot"
   (need-painhot ?) ; The LHS is if painhot is needed
   =>
   ; Call validatedAsk to ask if the person has pain when eating/drinking something hot
   (assert (painhot (validatedAsk "Do you have pain when eating/drinking something hot? ")))
)

/*
* This rule will assert yes/no for the characteristic paincold
* (whether the person's has pain when eating/drinkingsomething cold).
*
* If the value of paincold is needed, then the validatedAsk function is called, passing
* in "Do you have pain when eating/drinking something cold? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with paincold.
*/
(defrule need-paincold-rule "Rule to backward chain the characteristic paincold"
   (need-paincold ?) ; The LHS is if paincold is needed
   =>
   ; Call validatedAsk to ask if the person has pain when eating/drinking something cold
   (assert (paincold (validatedAsk "Do you have pain when eating/drinking something cold? ")))
)

/*
* This rule will assert yes/no for the characteristic prevcavities
* (whether the person's has previously had cavities).
*
* If the value of prevcavities is needed, then the validatedAsk function is called, passing
* in "Have you previously had cavities? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with prevcavities.
*/
(defrule need-prevcavities-rule "Rule to backward chain the characteristic prevcavities"
   (need-prevcavities ?) ; The LHS is if prevcavities is needed
   =>
   ; Call validatedAsk to ask if the person has previously had cavities
   (assert (prevcavities (validatedAsk "Have you previously had cavities? ")))
)

/*
* This rule will assert yes/no for the characteristic teethpainful (whether there is pain
* in the teeth). It will also assert yes/no for the characteristics that must logically 
* follow if teeth are painful.
*
* If the value of teethpainful is needed, then the validatedAsk function
* is called, passing in "Do you feel pain in your teeth? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the person feels pain in their teeth, we also know for sure
* that ontooth is yes, since the primary affected place must be the tooth
* if there is pain there.  Thus, that known piece of information is asserted as well.
* Finally, (teethpainful ?a) is asserted.
*/
(defrule need-teethpainful-rule "Rule to backward chain the characteristic teethpainful"
   (need-teethpainful ?) ; The LHS is if teethpainful is needed
   =>
   ; Call validatedAsk to ask if the person has painful teeth, save to ?a
   (bind ?a (validatedAsk "Do you feel pain in your teeth? "))
   (if (= ?a yes) then
      (assert (ontooth yes)) ; Primary affected place is tooth
   )
   (assert (teethpainful ?a)) ; assert teethpainful with its value ?a
)

/*
* This rule will assert yes/no for the characteristic teethloose (whether the person
* has any loose teeth). It will also assert yes/no for the characteristics that must logically 
* follow if any teeth are loose.
*
* If the value of teethloose is needed, then the validatedAsk function
* is called, passing in "Do you have any loose teeth? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the person has loose teeth, we also know for sure
* that ontooth is yes, since the primary affected place must be the tooth
* if the tooth is loose.  Thus, that known piece of information is asserted as well.
* Finally, (teethloose ?a) is asserted.
*/
(defrule need-teethloose-rule "Rule to backward chain the characteristic teethloose"
   (need-teethloose ?) ; The LHS is if teethloose is needed
   =>
   ; Call validatedAsk to ask if the person has loose teeth, save to ?a
   (bind ?a (validatedAsk "Do you have any loose teeth? "))
   (if (= ?a yes) then
      (assert (ontooth yes)) ; Primary affected place is tooth
   )
   (assert (teethloose ?a)) ; assert teethloose with its value ?a
)

/*
* This rule will assert yes/no for the characteristic teethcolorchange (whether the color of any teeth
* has recently changed). It will also assert yes/no for the characteristics that must logically 
* follow if teeth color has recently changed.
*
* If the value of teethcolorchange is needed, then the validatedAsk function
* is called, passing in "Has the color of any of your teeth changed recently? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the person's teeth have changed color recently, we also know for sure
* that ontooth is yes, since the primary affected place must be the tooth
* if there is color change there.  Thus, that known piece of information is asserted as well.
* Finally, (teethcolorchange ?a) is asserted.
*/
(defrule need-teethcolorchange-rule "Rule to backward chain the characteristic teethcolorchange"
   (need-teethcolorchange ?) ; The LHS is if teethcolorchange is needed
   =>
   ; Call validatedAsk to ask if the person has has a recent change in the color of their teeth, save to ?a
   (bind ?a (validatedAsk "Has the color of any of your teeth changed recently? "))
   (if (= ?a yes) then
      (assert (ontooth yes)) ; Primary affected place is tooth
   )
   (assert (teethcolorchange ?a)) ; assert teethcolorchange with its value ?a
)

/*
* This rule will assert yes/no for the characteristic puffysores
* (whether the person has sores that are puffy).
*
* If the value of puffysores is needed, then the validatedAsk function is called, passing
* in "Do you have puffy sores? " as the ?question. The output of validatedAsk (yes/no),
* is saved in the variable ?a.
* If ?a is yes (ie, the person has puffy sores, then we also know for sure that
* sores is yes, since having sores is a prerequisite to having puffy sores. Thus, that
* known piece of information is asserted as well. Finally, (puffysores ?a) is asserted
* as well.
*/
(defrule need-puffysores-rule "Rule to backward chain the characteristic puffysores"
   (need-puffysores ?) ; The LHS is if puffysores is needed
   =>
   ; Call validatedAsk to ask if the person has sores that are puffy, assert output with puffysores
   (bind ?a (validatedAsk "Do you have puffy sores? "))
   ; If the person has puffy sores, then we also know some more info
   (if (= ?a yes) then
      (assert (sores yes)) ; The patient must have sores if they have puffy sores
   )
   (assert (puffysores ?a))
)

(oralhealth) ; Run the file