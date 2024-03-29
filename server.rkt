#lang racket
 
(require web-server/servlet)
(provide/contract (start (request? . -> . response?)))

(require web-server/formlets
         "blog-model.rkt")

(define (start request)
  (render-blog-page
   (initialize-blog!
    (build-path (current-directory-for-user)
                "the-blog-data.sqlite"))
  request))

(define new-post-formlet
  (formlet
   (#%# ,{input-string . => . title}
        ,{input-string . => . body})
   (values title body)))
 
(define (render-blog-page a-blog request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "My Blog")
                  (link ((rel "stylesheet")
                       (href "/style.css")
                       (type "text/css"))))
            (body
             (h1 "My Blog")
             ,(render-posts a-blog embed/url)
             (form ([action
                     ,(embed/url insert-post-handler)])
                   ,@(formlet-display new-post-formlet)
                   (input ([type "submit"])))))))
 
  (define (insert-post-handler request)
    (define-values (title body)
      (formlet-process new-post-formlet request))
    (blog-insert-post! a-blog title body)
    (render-blog-page a-blog (redirect/get)))
  (send/suspend/dispatch response-generator))

(define new-comment-formlet
  input-string)

(define (render-post-detail-page a-blog a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Post Details")
                  (link ((rel "stylesheet")
                       (href "/style.css")
                       (type "text/css"))))
            (body
             (h1 "Post Details")
             (h2 ,(post-title a-post))
             (p ,(post-body a-post))
             ,(render-as-itemized-list
               (post-comments a-post))
             (form ([action
                     ,(embed/url insert-comment-handler)])
                   ,@(formlet-display new-comment-formlet)
                   (input ([type "submit"])))
             (a ([href ,(embed/url back-handler)])
                "Back to the blog")))))
 
  (define (insert-comment-handler request)
    (render-confirm-add-comment-page
     a-blog
     (formlet-process new-comment-formlet request)
     a-post
     request))
 
  (define (back-handler request)
    (render-blog-page a-blog request))
  (send/suspend/dispatch response-generator))

(define (render-confirm-add-comment-page a-blog a-comment
                                         a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Add a Comment")
                  (link ((rel "stylesheet")
                       (href "/style.css")
                       (type "text/css"))))
            (body
             (h1 "Add a Comment")
             "The comment: " (div (p ,a-comment))
             "will be added to "
             (div ,(post-title a-post))
 
             (p (a ([href ,(embed/url yes-handler)])
                   "Yes, add the comment."))
             (p (a ([href ,(embed/url cancel-handler)])
                   "No, I changed my mind!"))))))
 
  (define (yes-handler request)
    (post-insert-comment! a-blog a-post a-comment)
    (render-post-detail-page a-blog a-post (redirect/get)))
 
  (define (cancel-handler request)
    (render-post-detail-page a-blog a-post request))
  (send/suspend/dispatch response-generator))

(define (render-post a-blog a-post embed/url)
  (define (view-post-handler request)
    (render-post-detail-page a-blog a-post request))
  `(div ([class "post"])
        (a ([href ,(embed/url view-post-handler)])
           ,(post-title a-post))
        (p ,(post-body a-post))
        (div ,(number->string (length (post-comments a-post)))
             " comment(s)")))
 
(define (render-posts a-blog embed/url)
  (define (render-post/embed/url a-post)
    (render-post a-blog a-post embed/url))
  `(div ([class "posts"])
        ,@(map render-post/embed/url (blog-posts a-blog))))

(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths
               (list (build-path (current-directory-for-user) "static"))
               #:servlet-path
               "/servlets/server.rkt")
