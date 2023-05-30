#lang sicp
 (controller  
  (assign p (const 1)) 
  (assign c (const 1)) 
  
  test-n 
  (test (op >) (reg c) (reg n)) 
  (branch (label done)) 
  (assign x (op *) (reg p) (reg c)) 
  (assign y (op +) (reg c) (const 1)) 
  (assign p (reg x)) 
  (assign c (reg y)) 
  (goto (label test-n)) 
  
  done) 