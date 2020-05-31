/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2014                              *
 *  Authors: Ulrich Kremer                   *
 *           Hans Christian Woithe           *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"
typedef struct node{
  struct node* next;
  int val;

}node;
typedef struct list{
  node* head;
  node* tail;
  int size;
}list;
list fields;
void add(int val){
  node* t=(node *)malloc(sizeof(node));
  t->val=val;
  t->next =NULL; 
  if(fields.size==0){
    fields.head=t;
    fields.tail=t;
    fields.size=1;
  }
  else{
    fields.tail->next=t;
    fields.tail=fields.tail->next;
    fields.size=fields.size+1;
  }
}
int search(int val){
  node* p=fields.head;
  while(p!=NULL){
    if(p->val==val)
      return 1;
    p=p->next;
  }
  return 0;
}
void removef(int val){
  node* prev=NULL;
   node* f;
  node* p=fields.head;
  while(p!=NULL){
    if(p->val==val){
      if(prev==NULL){
		f=fields.head;
	fields.head=fields.head->next;
		free(f);
      }
      else if(p->next==NULL){
		f=p;
	prev->next=NULL;
	fields.tail=prev;
		free(f);
      }
      else{
		f=p;
	prev->next=p->next;
		free(f);
      }
      fields.size=fields.size-1;
      break;
    }
    prev=p;
    p=p->next;
  }
}
int main()
{
Instruction *head;
	
  fields.head=NULL;
  fields.tail=NULL;
  fields.size=0;
	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	/* YOUR CODE GOES HERE */
	Instruction *ptr=head;
	while(ptr!=NULL){
	  if(ptr->opcode==READ){
	    ptr->critical=1;
	  }
	  else
	    ptr->critical=0;
	  ptr=ptr->next;
	}
	ptr=head;
	while(ptr!=NULL){
	  if(ptr->opcode==WRITE){
	    ptr->critical=1;
	    Instruction *w=ptr->prev;
	    add(ptr->field1);
	    while(w!=NULL){
	      if(search(w->field1)){
		if(w->opcode==STORE||w->opcode==LOAD){
		  w->critical=1;
		  removef(w->field1);
		  add(w->field2);
		}
		else if(w->opcode==ADD||w->opcode==SUB||w->opcode==MUL||w->opcode==AND||w->opcode==OR){
		  w->critical=1;
		  removef(w->field1);
		  add(w->field2);
		  add(w->field3);
		}
		else if(w->opcode==LOADI){
		  w->critical=1;
		  removef(w->field1);
		}
	      }
	      w=w->prev;
	    }
	  }
	  ptr=ptr->next;
	}
	ptr=head;
	Instruction * f;
	Instruction * prev=NULL;
	while(ptr!=NULL){
	  if(ptr->critical==0){
	    if(prev==NULL){
	      f=head;
	      head=head->next;
	      free(f);
	      ptr=head;
	    }
	    else if(ptr->next==NULL){
	      f=ptr;
	      prev->next=NULL;
	      free(f);
	      ptr=NULL;
	    }
	    else{
	      f=ptr;
	      prev->next=ptr->next;
	      ptr=ptr->next;
	      free(f);
	    }
	  }
	  else{
	  prev=ptr;
	  ptr=ptr->next;
	  }
	}
	node* ff;
	while(fields.size>0){
	  ff=fields.head;
	  fields.head=fields.head->next;
	  free(ff);
	  fields.size=fields.size-1;
	}
	if (head) {
		PrintInstructionList(stdout, head);
		DestroyInstructionList(head);
	}
	return EXIT_SUCCESS;
}


