// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper','assert']"  --set exp.privatization none
typedef struct  {
	int i;
} S;

int main(){
	S ss;
	return ss.i; //WARN
}
