#!/usr/bin/scheme  --script 
(import
    (rnrs)
)

(define (xsetroot str)
    (system 
        (string-append
            "xsetroot -name "
            "\"" 
            str
            "\""
        )
    )
)
(define (->string c)
    (cond
      ((symbol? c) (symbol->string c))
      ((number? c) (number->string c))
      ((string? c) c)
      ("ERR ->string")
    )
)
(define (command->str str)
    (define read-tem 
        (lambda (return-str port)
            (let ((c (read port)))
                (if (eof-object? c)
                        (begin (close-port port)
                            return-str)
                        (read-tem (string-append return-str " " (->string c)) port)
                ))          
        )
    )
    (read-tem "" (car 
           (process str)))
)
(define (command->list str)
    (define (add-list l1 l2)
    (if (null? l1)
        l2
        (add-list (cdr l1) (cons (car l1) l2))))
    (define read-tem 
        (lambda (return-list port)
            (let ((c (read port)))
                (if (eof-object? c)
                        (begin (close-port port)
                            return-list)
                        (read-tem (add-list return-list  (list (->string c))) port)
                ))          
        )
    )
    (read-tem '() (car 
           (process str)))
)
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define pub_ip "0:0:0:0")
    (define pub-ip-size 0)
         (fork-thread   (rec thread
             (lambda ()
                (let (
                    (ip-list (command->list "curl -s --connect-timeout 1 -m 3 ifconfig.co"))
                )
                    (begin
                        (set! pub_ip (if (null? ip-list) 
                                                        "0:0:0:0"
                                                        (car ip-list)
                                                        ))
                        (if (and (equal? "0:0:0:0" pub_ip) (< pub-ip-size 12))
                            (begin 
                                (system "sleep 5s") 
                                (set! pub-ip-size (+ 1 pub-ip-size))
                            )
                            (begin
                                (system "sleep 60s") 
                                (set! pub-ip-size 0)
                            )
                        )
                        (thread)
                    )
                )
             )
           )
        )
(define (getPubIp)
    (string-append "" pub_ip)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (getLocalIp)
    (string-append "ﯱ "  (car (command->list "ip addr | grep -e \"inet \" | awk \'NR==2\' | sed 's/^.*inet.//g; s/\\/.*//g\'")))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define getDate
    (lambda ()
        (string-append  ""  (car (command->list "date '+%y-%m-%d'")))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (getTime)
    (string-append " " (car (command->list "date '+%H:%M:%S'")))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;声音
(define (getVolume)
    (let ((pair (command->list "amixer get Master | awk -F'[][]' 'END{ print $4\" \"$2 }'")))
        (string-append 
            (if (equal? (car pair) "off")
                ""
                ""
            ) 
            (cadr pair)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (getUserName)
    (car (command->list "echo $USER@$(hostname)"))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define upNet 0)
(define downNet 0)
(define (startNetThread)
    (define (downNetFun iface)
        (string->number (car (command->list  (string-append "ip -s -c link show " iface "| tail -n3 | head -n 1 | cut -d \" \" -f5"))))
    )
     (define (upNetFun iface)
        (string->number (car (command->list  (string-append "ip -s -c link show " iface "| tail -n1 | cut -d \" \" -f5"))))
    )
    (define (getExecList size fun l)
        (cond 
            ((null? l)
                size)
            ((equal? "lo" (car l)) 
                (getExecList size fun (cdr l)))
            ((getExecList    (+ 
                                            size
                                            (fun (car l))
                                        ) 
                                        fun 
                                        (cdr l)))
        )
    )
    (fork-thread   (rec thread
             (lambda ()
                        (let* (
                            (equipments (command->list "ip -o link show | awk -F': ' '{print $2}'"))
                            (upNetTem (getExecList 0 upNetFun equipments))
                            (downNetTem (getExecList 0 downNetFun equipments))
                        )
                    (begin
                         (system "sleep 1s")      
                        (set! upNet (/ 
                            (-
                                (getExecList 0 upNetFun equipments) 
                                upNetTem)
                             1024))
                        (set! downNet (/ 
                            (-
                                (getExecList 0 downNetFun equipments) 
                                downNetTem)
                             1024))
                        (thread)
                    )
                )
           )
        )
    )
)
(startNetThread)
(define (getNetDownUp)
   (string-append "" (number->string (truncate downNet)) "KB/s " (number->string (truncate upNet)) "KB/s")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (getBattery)
    (let (
        (capacity (string->number (car (command->list "cat /sys/class/power_supply/BAT0/capacity"))))
        (charging (command->str "cat /sys/class/power_supply/BAT0/status"))
    )
    (string-append 
         (cond
           ((equal? "Charging" charging) "")
             ((> 20 capacity) "")
             ("")
       )
        (cond
            ((> capacity 95) "")
            ((> capacity 70) "")
            ((> capacity 45) "")
            ((> capacity 20) "")
            ("")
        )
        (number->string capacity)
        "%"
    )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (getRam)
    (let (
        (use (car (command->list "free -h | awk '(NR==2){ print $3 }'")))
        (all (car (command->list "free -h | awk '(NR==2){ print $2 }'")))
    )
    (string-append
        ""
        use
        "/"
        all
    ))    
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (getCputemp)
    (string-append
        ""
        (command->str "sed 's/000$/°C/' /sys/class/thermal/thermal_zone0/temp")
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cpu 0)
 (fork-thread   (rec thread
            (lambda ()
                (let* (
                    (pair (command->list "vmstat 1 2 | tail -1"))
                    (tem (caddr (cddddr (cddddr(cddddr pair)))))
                )
                (begin
                    (set! cpu (+
                        (string->number (car pair))
                        (string->number tem)
                    ))
                    (thread)
                )
        )
    )
))
(define (getCpu)
    (string-append 
        ""
        (number->string cpu)
        "%"
    )
)

(define (main)
    (begin
        (xsetroot 
            (string-append
                (getNetDownUp)
                " | "
                (getDate)
                " | "
                (getTime)
                " | "
                (getVolume)
                " | "
                 (getUserName)
                " | "
                 (getBattery)
             "   ; "
                (getLocalIp)
                " | "
                 (getCpu)
                " | "
                 (getRam)
                " | "
                (getCputemp)
                " | "
                (getPubIp)
             )
        )
        (system "sleep 1")
        (main)
    )
)
(main)




;echo '(compile-file "bar.ss")  (make-boot-file "bar" (quote ("petite")) "bar.so")' | scheme -q