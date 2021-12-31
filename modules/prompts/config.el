;;; prompts/config.el -*- lexical-binding: t; -*-

(defvar my-projects
  '("chores"
    "lectures"))



(defvar prompt-phrases (list
                        (cons "What meaningfull or important thing should you tell a particular person that you havent't said to them yet?" (cons 1 1))
                        (cons "Think about things you like about other people" (cons 1 1))
                        (cons "If you could go back in time and change one thing about your past, what would it be?" (cons 1 1))
                        (cons "What did I do today that was fun?" (cons 1 1))
                        (cons "What would you do, if you knew you could not fail?" (cons 1 1))
                        (cons "Write a 'thank you' letter to someone" (cons 1 1))
                        (cons "If you could have dinner with anyone currently alive, who would it be?" (cons 1 1))
                        (cons "What are you looking forward to the most?" (cons 1 1))
                        (cons "What surprised you today?" (cons 1 1))
                        (cons "What did I notice today?" (cons 1 1))
                        (cons "What is the most outrageous thing you did recently?" (cons 1 1))
                        (cons "Which past experience are you the most thankfull for? Why?" (cons 1 1))
                        (cons "What’s a brave thing you did last week?" (cons 1 1))
                        (cons "When was I at peace today?" (cons 1 1))
                        (cons "What have been your biggest mistakes recently? What have you learned from them?" (cons 1 1))
                        (cons "How did you feel connected to others today?" (cons 1 1))
                        (cons "What was something playfull you did today?" (cons 1 1))
                        (cons "What battles have you fought and overcome in your life?" (cons 1 1))
                        (cons "How would you like to spend your spare time?" (cons 1 1))
                        (cons "What would you do if money were no object?" (cons 1 1))
                        (cons "What’s your secret desire?" (cons 1 1))
                        (cons "What made me feel energized today?" (cons 1 1))
                        (cons "What opportunity presented itself today?" (cons 1 1))
                        (cons "What made me appreciate my city, state or country today?" (cons 1 1))
                        (cons "Who was I happy to meet with, chat with, or run into today?" (cons 1 1))
                        (cons "How was I able to help others today" (cons 1 1))
                        (cons "What compliments did I receive today?" (cons 1 1))
                        (cons "What problem was I able to resolve today?" (cons 1 1))
                        (cons "What was one small victory I had today?" (cons 1 1))
                        (cons "How did you feel, when you woke up today?" (cons 1 1))
                        (cons "What has did you accomplish today?" (cons 1 1))
                        (cons "What was the biggest turning point in your life, and how did that experience change you?" (cons 1 1))
                        (cons "What simple pleasure did I enjoy today?" (cons 1 1))
                        (cons "What could you do to bring more of what really excites you into your life?" (cons 1 1))
                        (cons "Summarized in just a few sentences, what is your life's story?" (cons 1 1))
                        (cons "What would you like the next chapter of this story to be?" (cons 1 1))
                        (cons "What would you say is the greatest accomplishment of your life so far? Brag for a minute." (cons 1 1))
                        (cons "What do you want to make sure you do, achieve, or experience before you're gone?" (cons 1 1))
                        (cons "In recent years, what's the biggest lesson you've learned about yourself?" (cons 1 1))
                        (cons "Who inspires you most, and why do you find them inspiring?" (cons 1 1))
                        (cons "What was the biggest turning point in your life, and how did that experience change you?" (cons 1 1))
                        (cons "What are you taking for granted that you want to remember to be grateful for?" (cons 1 1))
                        (cons "Think for a moment about the biggest problem right now in your life. If that problem was happening to a close friend instead of to you, what would you say to comfort or advise that friend?" (cons 1 1))
                        (cons "What meaningful or important thing should you tell a particular person that you haven't said to them yet?" (cons 1 1))
                        (cons "When are you going to tell this person this meaningful or important thing?" (cons 1 1))
                        (cons "What's one of the best days you've had in your entire life? Describe what happened that day." (cons 1 1))
                        (cons "What in your life that you have the power to change is most limiting your long-term happiness?" (cons 1 1))
                        (cons "What could you start doing now to address what you said is most limiting your happiness?" (cons 1 1))
                        (cons "If you had to have roughly the same work day, 5 days a week, for the next 10 years, what activities would you ideally want this work day to consist of?" (cons 1 1))
                        (cons "What can you do to make your current job closer to this ideal, or to help you get a job that is closer to this ideal?" (cons 1 1))
                        (cons "What is the most important thing that you know you really should do but which you have trouble getting yourself to do?" (cons 1 1))
                        (cons "What could you do now to make it more likely that you actually do this important thing?" (cons 1 1))
                        (cons "What do you think is holding you back from achieving more in your life than you've achieved so far?" (cons 1 1))
                        (cons "What could you start doing now that would help address what you said is holding you back in life?" (cons 1 1))
                        (cons "In your opinion, what is the purpose or meaning of life?" (cons 1 1))
                        (cons "How is the best version of yourself different from the way you sometimes behave?" (cons 1 1))
                        (cons "What has kept you hopeful in life's most challenging moments?" (cons 1 1))
                        (cons "During what period of your life were you the happiest, and why were you so happy then?" (cons 1 1))
                        (cons "Imagine that you received a message from a version of yourself five years in the future. What warnings would the message give you, and what advice would it offer about how best to achieve your goals?" (cons 1 1))
                        (cons "If you knew for a fact that you were going to die exactly 10 years from now, how would you change your current behavior?" (cons 1 1))
                        (cons "Suppose you knew that you were going to die instantly (but painlessly) in exactly 7 days. What would you spend your last week doing?" (cons 1 1))
                        (cons "If you could plan one nearly perfect (but still actually realistic) day for yourself, what would you spend that day doing? Describe that day, from when you wake up until you go to sleep." (cons 1 1))
                        (cons "When is the soonest that you can treat yourself to this perfect day, or to another day that you'll really enjoy and remember?" (cons 1 1))
                        ))

(defun t/random-phrase ()
    (interactive)
      (while (progn
               (setq t/last (seq-random-elt prompt-phrases))
               (< (* (car (cdr (calcFunc-random '(float 1 0)))) (expt 10 -12))
                                (/(car (last t/last))
                        (float (+ (car (last t/last))
                                  (cdr (last t/last)))))) ;s+1 / n+2 see laplace rule of succession.
    (insert (car t/last)))))
(defun t/incr-last ()
        (interactive)
        (setcar (last t/last)
        (1+ (car (last t/last)))))
(defun t/decr-last ()
        (interactive)
        (setcdr (last t/last)
        (1+ (cdr (last t/last)))))


(setq desktop-globals-to-save
      '(desktop-missing-file-warning
    tags-file-name
    tags-table-list
    search-ring
    regexp-search-ring
    register-alist
    file-name-history
    prompt-phrases))

(defun save-phrases ()
  "save-phrases with their usage-data"
  (interactive)
  (with-temp-buffer
    (insert (prin1-to-string prompt-phrases))
    (write-region (point-min)
                  (point-max)
                  "prompt-data.el")))
