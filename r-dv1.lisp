
;(input-edge-data *edge-data-2*  e1 #(g f v))
;(push (cons 't1 '((e1 e2 e3 e4) rt free)) *t-list-2*)
;(clrhash ht)  清空哈希表
;(remhash "apple" ht) 删除哈希表中一个键值

;edge-data
;(input-edge-data *edge-data-2*  e1 #(c a v))
;(input-edge-data *edge-data-2*  e2 #(c e h))
;(input-edge-data *edge-data-2*  e3 #(e f v))
;(input-edge-data *edge-data-2*  e4 #(a f h))
;(input-edge-data *edge-data-2*  e5 #(c f v))
;(input-edge-data *edge-data-2*  e6 #(a b h))
;(input-edge-data *edge-data-2*  e7 #(f b v))
;(input-edge-data *edge-data-2*  e8 #(d b v))
;(input-edge-data *edge-data-2*  e9 #(f d h))
;(input-edge-data *edge-data-2*  e10 #(e d h))


;(input-edge-data *edge-data-2*  e1 #(d a v))
;(input-edge-data *edge-data-2*  e2 #(d e h))
;(input-edge-data *edge-data-2*  e3 #(b e h))
;(input-edge-data *edge-data-2*  e4 #(a b h))
;(input-edge-data *edge-data-2*  e5 #(d b v))
;(input-edge-data *edge-data-2*  e6 #(e f h))
;(input-edge-data *edge-data-2*  e7 #(f c v))
;(input-edge-data *edge-data-2*  e8 #(b c h))
;(input-edge-data *edge-data-2*  e9 #(e c v))


(defun return-next (x)       ;用于直接返回值
  x)


(defvar *edge-data-2* (make-hash-table))                        ;哈希表用以存放各边数据


(defun other-edge-of-boundary (sp-list ep-list direction)        ;找到回转边的另一侧同方向边,由此确认构成了新的T构造边界
  (let (other-point)
    (dolist (sp sp-list)                                         ;与回转边两端相接的两种顶点,一一连接确认是否存在新的边界
      (dolist (ep ep-list)
	(maphash #'(lambda (key value)
		     (if (and (eql sp (elt value 0)) (eql ep (elt value 1)) (eql direction (elt value 2)))
			 (setf other-point (list (aref value 0) (aref value 1))))) 
		 *edge-data-2*)))
    (return-next other-point)))                                 ;若存在,返回这条边的两个顶点(方向与回转边一致,所以无需赘述)



(defun new-boundary (start-or-end point direction)              ;获取回转边的一个顶点,以及位置情况,用以确认是否存在反向的两条边界
  (if (eql start-or-end 'end)                        ;作为T边界的上边时,寻找与其点相接的T边界的左右边
      (progn
	(let (point-list) (maphash #'(lambda (key value)
				       (if (and (eql (elt value 1) point) (eql (elt value 2) direction))
					   (push (elt value 0) point-list)))
				   *edge-data-2*)
	     (return-next point-list)))
      (progn
	(let (point-list) (maphash #'(lambda (key value)
				       (if (and (eql (elt value 0) point) (eql (elt value 2) direction))
					   (push (elt value 1) point-list)))
				   *edge-data-2*)
	     (return-next point-list)))))
	   


(defvar *new* 0)              ;新T构造的个数


(defvar *t-list-2* nil)       ;存放T构造


(defmacro input-edge-data (edge-data edge array)     ;输入边的宏
  `(setf (gethash ',edge ,edge-data) ,array))


(defun output-edge-data (edge-data)                   ;输出每条边
  (maphash #'(lambda (key value)
	       (format t "~a => ~a~%" key value))
	   edge-data))

(defvar *r-duals* 0)         ;矩形对偶的个数



(defun change-rt-to-lt (rt-edge-list edge-data)    ;由RT回转为LT                
  (dolist (one-edge rt-edge-list)                     
    (maphash #'(lambda (key value)              
		 (if (eql one-edge key)         
		     (cond                     
		       ((eql (elt value 2) 'h)
			(progn
			  (rotatef (aref value 0) (aref value 1))
			  (setf (elt value 2) 'v)))
		       ((eql (elt value 2) 'v)
			(progn
			  (setf (elt value 2) 'h)
			  )))))
	     edge-data)))



(defun change-lt-to-rt (lt-edge-list edge-data)          ;LT回转为RT          
  (dolist (one-edge lt-edge-list)                    
    (maphash #'(lambda (key value)              
		 (if (eql one-edge key)         
		     (cond                     
		       ((eql (elt value 2) 'v)
			(progn
			  (rotatef (aref value 0) (aref value 1))
			  (setf (elt value 2) 'h)))
		       ((eql (elt value 2) 'h)
			(setf (elt value 2) 'v)))))				 
	     edge-data)))



(defun test-rt-free (t-list p edge-data)                ;对T列表中所有T构造进行操作
  (if (and (find 'free (nth p t-list)) (find 'rt (nth p t-list)))
      (progn
	(setf (third (nth p t-list)) 'lt)
	(let ((temp-t-list (copy-tree t-list)))
	  (change-rt-to-lt (second (nth p t-list)) edge-data)    ;(nth p t-list)为t-list中第p位的T构造,对其旋转  
	  (check-each-edge (second (nth p t-list)) edge-data temp-t-list)    ;旋转后检查T构造的各边
	  (test-rt-free temp-t-list (+ p 1) edge-data))
	(setf (third (nth p t-list)) 'rt)
	(change-lt-to-rt (second (nth p t-list)) edge-data)
	(setf (fourth (nth p t-list)) 'notfree)
	(test-rt-free (copy-tree t-list) (+ p 1) edge-data))
      (progn
	(mapc #'(lambda (output)
		  (format t "~a~%" output))
	      t-list)
	;(output-edge-data edge-data)   ;输出每条边
	(format t "*************~%~%")
	(setf *r-duals* (+ *r-duals* 1)))))




(defun check-each-edge (changed-edge-list edge-data t-list)    ;对旋转后RT构造的每条边进行检查:是否因旋转产生了新的T构造
  (dolist (one-edge changed-edge-list)                         ;从列表中依次取出每条边进行检查
    (maphash #'(lambda (key value)
		 (if (eql one-edge key)
		     (progn
		       (format t "change edge : ~a , " key) 
		       (find-new-t (elt value 0) (elt value 1) (elt value 2) t-list)     ;RT的边回转后需要确认是否构成了新的T边界
		       )))
	     edge-data)))




(defun find-new-t (start-point end-point direction t-list)                ;获取回转边的两个端点和方向,以及当前的T-list来确认是否存在新的T边界
  (when (eql direction 'h)
    (format t "v -> h~%")
    (progn
      (format t "if this edge is the upper boundary of T boundary :~%")
      (let ((sp-list (new-boundary 'end start-point 'v)) (ep-list (new-boundary 'end end-point 'v)))  ;end用来表示与边界两边的关系
	(let ((boundary-point (append (list start-point end-point) (other-edge-of-boundary sp-list ep-list direction))))
	  (if (and (first boundary-point) (second boundary-point) (third boundary-point) (fourth boundary-point)) ;四个边界点不为空
	      (progn
		(h-on-top (first boundary-point) (second boundary-point) (third boundary-point) (fourth boundary-point) t-list)
		(format t "Each vertex of the boundary is (top to bottom): ~a ~a ~a ~a~%~%" (first boundary-point) (second boundary-point) (third boundary-point) (fourth boundary-point)))
	      (format t "No new T was found~%~%")))))
    (progn
      (format t "if this edge is the lower boundary of T boundary :~%")
      (let ((sp-list (new-boundary 'start start-point 'v)) (ep-list (new-boundary 'start end-point 'v)))  ;end用来表示与边界两边的关系
	(let ((boundary-point (append (list start-point end-point) (other-edge-of-boundary sp-list ep-list direction))))
	  (if (and (first boundary-point) (second boundary-point) (third boundary-point) (fourth boundary-point)) ;四个边界点不为空
	      (progn
		(h-on-top (third boundary-point) (fourth boundary-point) (first boundary-point) (second boundary-point) t-list)
		(format t "Each vertex of the boundary is (top to bottom): ~a ~a ~a ~a~%~%" (third boundary-point) (fourth boundary-point) (first boundary-point) (second boundary-point)))
	      (format t "No new T was found~%~%"))))))


  (when (eql direction 'v)
    (format t "h -> v~%")
    (progn
      (format t "if this edge is the left boundary of T boundary :~%")
      (let ((sp-list (new-boundary 'start start-point 'h)) (ep-list (new-boundary 'start end-point 'h)))
	(let ((boundary-point (append (list start-point end-point) (other-edge-of-boundary sp-list ep-list direction))))
	  (if (and (first boundary-point) (second boundary-point) (third boundary-point) (fourth boundary-point))
	      (progn
		(h-on-top (second boundary-point) (fourth boundary-point) (first boundary-point) (third boundary-point) t-list)
		(format t "Each vertex of the boundary is (left to right): ~a ~a ~a ~a~%~%" (first boundary-point) (second boundary-point) (third boundary-point) (fourth boundary-point)))
	      (format t "No new T was found~%~%")))))

    (progn
      (format t "if this edge is the right boundary of T boundary :~%")
      (let ((sp-list (new-boundary 'end start-point 'h)) (ep-list (new-boundary 'end end-point 'h)))
	(let ((boundary-point (append (list start-point end-point) (other-edge-of-boundary sp-list ep-list direction))))
	  (if (and (first boundary-point) (second boundary-point) (third boundary-point) (fourth boundary-point))
	      (progn
		(h-on-top (fourth boundary-point) (second boundary-point) (third boundary-point) (first boundary-point) t-list)
		(format t "Each vertex of the boundary is (left to right): ~a ~a ~a ~a~%~%" (third boundary-point) (fourth boundary-point) (first boundary-point) (second boundary-point)))
	      (format t "No new T was found~%~%")))))))
		



(defun h-on-top (sp-1 ep-1 sp-2 ep-2 t-list)          ;得到T边界的四个顶点,不同回转边对应的顶点顺序也不同
  (let (new-t-list) 
    (maphash #'(lambda (key value)
		 (cond
		   ((and (eql (elt value 0) sp-1) (eql (elt value 2) 'h))   ;(g->other)h
		    (unless (eql (elt value 1) ep-1)                        ;other 不等于 h
		      (if (inside-t (elt value 1) ep-2 'left nil)           ;以other为起始点,T边界的对角点为目标,检查是否在T边界内部
			  (push key new-t-list))))
		   ((and (eql (elt value 1) ep-1) (eql (elt value 2) 'v))   ;(other->h)v
		    (unless (eql (elt value 0) ep-2)                        ;other 不等于 k
		      (if (inside-t (elt value 0) sp-2 'top nil)            ;以other为起始点,T边界的对角点为目标,检查是否在T边界内部
			  (push key new-t-list))))
		   ((and (eql (elt value 0) sp-2) (eql (elt value 2) 'v))   ;(j->other)v
		    (unless (eql (elt value 1) sp-1)                        ;other 不等于 g
		      (if (inside-t (elt value 1) ep-1 'bottom nil)
			  (push key new-t-list))))
		   ((and (eql (elt value 1) ep-2) (eql (elt value 2) 'h))   ;(other->k)h
		    (unless (eql (elt value 0) sp-2)                        ;other 不等于 j
		      (if (inside-t (elt value 0) sp-1 'right nil)
			  (push key new-t-list))))))
	     *edge-data-2*)
    (if (eql new-t-list nil)
	(format t "not rt~%")
	(progn
	  (push (cons *new* (list new-t-list 'rt 'free)) (cdr (last t-list)))
	  (format t "Find a new T-structure : ~a , append it to *t-list-2*~%" (cons *new* (list new-t-list 'rt 'free)))
	  (setf *new* (+ *new* 1))))))


(defun start (t-list edge-data)         ;主函数
  (test-rt-free (copy-tree t-list) 0 edge-data)       ;使用copy-tree来确保对T-list的操作只会影响子节点,不会影响父节点
  (format t "Sums of R-duals = ~a~%" *r-duals*)
  (setf *r-duals* 0)
  (setf *new* 0))



(defun inside-t (start target direction return-num)             ;检查此顶点是否在T边界内部,target为T边界的对角顶点
  (when (eql direction 'left)                                   ;direction为边界顶点所处位置(top,bottom,left,right)
    (maphash #'(lambda (key value)
		 (if (and (eql (elt value 0) start) (eql (elt value 2) 'h))    ;在左边时,向水平方向检查每条边的end point是否为目标
		     (progn
		       (if (eql (elt value 1) target)
			   (progn
			     (format t "found target : ~a~%" target)
			     (setf return-num 1))                             ;如果找到目标则返回1,否则返回nil
			   (progn
			     (let ((x (inside-t (elt value 1) target direction return-num)))  ;如果还有水平方向边,继续递归查找
			       (format t "x = ~a, start = ~a~%" x start)
			       (if (eql 1 x)
				   (setf return-num 1)
				   (format t "not found target"))))
			   ))))
	     *edge-data-2*))
  
  (when (eql direction 'right)
    (maphash #'(lambda (key value)
		 (if (and (eql (elt value 1) start) (eql (elt value 2) 'h))
		     (progn
		       (if (eql (elt value 0) target)
			   (progn
			     (format t "found target : ~a~%" target)
			     (setf return-num 1))
			   (progn
			     (let ((x (inside-t (elt value 0) target direction return-num)))
			       (format t "x = ~a, start = ~a~%" x start)
			       (if (eql 1 x)
				   (setf return-num 1)
				   (format t "not found target"))))
			   ))))
	     *edge-data-2*))
  
  (when (eql direction 'top)
    (maphash #'(lambda (key value)
		 (if (and (eql (elt value 1) start) (eql (elt value 2) 'v))
		     (progn
		       (if (eql (elt value 0) target)
			   (progn
			     (format t "found target : ~a~%" target)
			     (setf return-num 1))
			   (progn
			     (let ((x (inside-t (elt value 0) target direction return-num)))
			       (format t "x = ~a, start = ~a~%" x start)
			       (if (eql 1 x)
				   (setf return-num 1)
				   (format t "not found target"))))
			   ))))
	     *edge-data-2*))
  
  (when (eql direction 'bottom)
    (maphash #'(lambda (key value)
		 (if (and (eql (elt value 0) start) (eql (elt value 2) 'v))
		     (progn
		       (if (eql (elt value 1) target)
			   (progn
			     (format t "found target : ~a~%" target)
			     (setf return-num 1))
			   (progn
			     (let ((x (inside-t (elt value 1) target direction return-num)))
			       (format t "x = ~a, start = ~a~%" x start)
			       (if (eql 1 x)
				   (setf return-num 1)
				   (format t "not found target"))))
			   ))))
	     *edge-data-2*))
  
  (if (eql start target)    ;one edge of T
      (setf return-num 1))
  
  (return-next return-num))
