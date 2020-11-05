
  * Stack grows up; pointer is incremented when new data is pushed.
  * Kempe data pointer is maintained through calls.
  * `kempe_init` allocates the Kempe stack in RAM using `malloc`, placing
    a pointer to the Kempe in the appropriate register.
  * `kempe_cleanup` frees the Kempe stack

# x86

  * `rbp` has Kempe data pointer.

# Aarch64

  * `x19` has Kempe data pointer.
  <!-- `r18` maybe? -->
