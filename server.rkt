#lang web-server/insta
 
(struct post (title body comments) #:mutable)

(struct blog (posts) #:mutable)
 
(define BLOG
  (list (post "Second Post" "This is another post" '("Comment one" "Comment two"))
        (post "First Post" "This is my first post" '())))

(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))

(define (post-insert-comment! a-post a-comment)
  (set-post-comments! a-post
                      (cons a-comment (post-comments a-post))))
 
(define (start request)
  (render-blog-page BLOG request))
 
(define (render-blog-page a-blog request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "My Blog"))
            (body
             (h1 "My Blog")
             ,(render-posts a-blog)
             (form ((action
                     ,(embed/url insert-post-handler)))
              (label "Title:" (input ((name "title"))))
              (label "Body Content:" (textarea ((name "body"))))
              (input ((type "Submit"))))))))

  (define (parse-post bindings)
    (post (extract-binding/single 'title bindings)
          (extract-binding/single 'body bindings)
          (list)))
  
  (define (insert-post-handler request)
    (blog-insert-post!
     BLOG (parse-post (request-bindings request)))
    (render-blog-page))
    
  (send/suspend/dispatch response-generator))

(define (render-post-detail-page a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title (string-append (post-title a-post) " | RFR Blog")))
            (body
             (h1 ,(post-title a-post))
             (p ,(post-body a-post))
             ,(render-as-itemized-list
               (post-comments a-post))
             (form ((action
                     ,(embed/url insert-comment-handler)))
                   (textarea ((name "comment"))
                             ((placeholder "Share your thoughts...")))
                   (input ((type "submit"))))))))
  (define (parse-comment bindings)
    (extract-binding/single 'comment bindings))

  (define (insert-comment-handler a-request)
    (post-insert-comment!
     a-post (parse-comment (request-bindings a-request)))
    (render-post-detail-page a-post a-request))
  
  (send/suspend/dispatch response-generator))

(define (render-post a-post embed/url)
  (define (view-post-handler request)
    (render-post-detail-page a-post request))
  `(div ((class "post"))
        (a ((href ,(embed/url view-post-handler)))
           ,(post-title a-post))
        (p ,(post-body a-post))
        (div ,(number->string (length (post-comments a-post)))
             " comment(s)")))
 
(define (render-posts embed/url)
  (define (render-post/embed/url a-post)
    (render-post a-post embed/url))
  `(div ((class "posts"))
        ,@(map render-post/embed/url (blog-posts BLOG))))

(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))
