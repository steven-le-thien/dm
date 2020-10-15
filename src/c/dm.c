#include <cstdio>
#include <iostream>
#include <cstdlib>

#include "dm.h"
#include "utilities.h"

int dm(int *leaves,
    int br_len_offset,
    int **distance,
    int new_root,
    Node **tree,
    int num_leaves)
{
  if(num_leaves == 0) return GENERAL_ERROR;
  else if (num_leaves == 1){
  	tree[leaves[0]]->par = new_root;
  	tree
  }
}

int main(){
}
