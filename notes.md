# Notes

- Currently the parser doesn't support calling functions that are members of objects, or inside of arrays. This seems like it should be possible, but I don't see any instances of it in the MW2 code.
- To do
    - debug blocks `/# code #/`
    - `%` statements
    ```
    self.primaryTurretAnim = %crouchSAWgunner_aim;
    ```
    - assignment and operation operators like `+=`, `-=`
