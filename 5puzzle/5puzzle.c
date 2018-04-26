//************************************************
// 　５パズル/ハッシュ法による幅優先探索
//************************************************
#include <stdio.h>

#define  FALSE   0
#define  TRUE    1

#define  BOARD_WIDTH 3 // ボード横長さ
#define  BOARD_SIZE  6 // ボードサイズ

//*******************************************
//  ボードの座標                            
//  +--+--+--+  　空きマスの位置へ移動出来る
//  |０|１|２|  駒の位置をテーブル化しておく
//  +--+--+--+  例えば空きマスの位置が０なら
//  |３|４|５|  移動出来る駒の位置は１と３。
//  +--+--+--+  　-1はデータの終わり(番兵)  
//*******************************************
int Moval[6][4] = {
    { 1, 3,-1, 0}, // <-空き位置が0なら移動可能位置は1,3  
    { 0, 2, 4,-1}, // <-空き位置が1なら移動可能位置は0,2,4
    { 1, 5,-1, 0}, // <-空き位置が2なら移動可能位置は1,5  
    { 0, 4,-1, 0}, //   以下同様に  
    { 1, 3, 5,-1},
    { 2, 4,-1, 0},
};
char BOARD[BOARD_SIZE];      // ボード

char Start[BOARD_SIZE] = {   // 開始局面
    4, 5, 3,
    1, 2, 0
};
char Finish[BOARD_SIZE] = {  // 終了局面
    1, 2, 3,
    4, 5, 0
};

//*******************************************
// ハッシュ表とキューの定義                 
//*******************************************
#define  HASH_SIZE   877  // 必要量の２割増し程度(出来れば素数)
#define  QUEUE_SIZE  720  // 待ち行列キューサイズ(実際の必要量)
#define  EMPTY        -1  // ハッシュ表の要素(バケット)が空の印

typedef struct bucket {
    char    board[BOARD_SIZE]; // ゲームボード
    int     space;           // 空きマスの座標(未使用ならEMPTY)
    struct  bucket *prev;    // 自分の親局面を示すポインタ
} bucket_t;
bucket_t HASH[HASH_SIZE];    // ハッシュ表(格納庫)

bucket_t *QUEUE[QUEUE_SIZE]; // 待ち行列(キュー)  
bucket_t *FINISH;            // 終了局面の保存番地
int  Qhead;                  // キューの先頭位置  
int  Qtail;                  // キューの後ろ位置  

//*******************************************
// 再帰呼び出しを使って解答手順を表示する   
//*******************************************
int Display(bucket_t *ptr)
{
    int  i, count=0;

    // 親局面があれば親局面を先に表示する(再帰呼び出し)
    if (ptr->prev) count = Display(ptr->prev);

    // この局面を表示する
    for (i=0; i<BOARD_SIZE; i++) {
        if (i % BOARD_WIDTH == 0) printf("\n");
        if (ptr->board[i]) printf(" %d",ptr->board[i]);
        else               printf("  ");
    }
    printf("  %d手目\n", count);

    return count + 1; // 呼び出元の手数を教える
}

//*******************************************
// 現在の局面をハッシュ表に登録する         
//*******************************************
void BucketIn(bucket_t *prev, bucket_t *address, int space)
{
    int  i;

    for (i=0; i<BOARD_SIZE; i++)
        address->board[i] = BOARD[i];
    address->space = space; // 空きマスの座標  
    address->prev  = prev;  // 親局面のポインタ
}

//*******************************************
// ハッシュ関数(局面の格納番地を求める)     
//*******************************************
bucket_t *HashFunc(void)
{
    bucket_t *ptr;
    unsigned long hash;
    int  i;

    // 局面を数値化しハッシュ値を求める
    hash = 0;
    for (i=0; i<BOARD_SIZE; i++)        // 2ビットシフトして加算する
        hash = (hash << 2) + BOARD[i];  // というメチャクチャな計算 
    hash %= HASH_SIZE;

    // 実際の格納場所を見つける
    for (;;) {
        ptr = &HASH[hash];              // 格納場所のポインタ
        if (ptr->space == EMPTY) break; // この場所は未使用

        // 同じ局面かをチェック
        for (i=0; i<BOARD_SIZE; i++)
            if (ptr->board[i] != BOARD[i]) break;
        if (i == BOARD_SIZE) break;     // 同じ局面である

        hash = (hash + 1) % HASH_SIZE;  // 再ハッシュ
    }
    return ptr; // 格納場所のポインタを返す
}

//*******************************************
//  待ち行列(キュー)を使った幅優先探索      
//*******************************************
int BreadthFirst(void)
{
    bucket_t *address1, *address2;
    int   i, space, piece;
    char  n;

    // ハッシュ表とキューの初期化
    for (i=0; i<HASH_SIZE; i++)
        HASH[i].space = EMPTY;
    Qhead = Qtail = 0;

    // 開始局面を登録(親局面の番地はNULL)
    for (i=0; i<BOARD_SIZE; i++)
        BOARD[i] = Start[i];
    address1 = HashFunc();        // 格納場所を求める
    BucketIn(NULL, address1, 5);  // ハッシュ表に登録
    QUEUE[Qtail++] = address1;    // キューに入れる  

    // 終了局面を登録(キューには入れない)
    for (i=0; i<BOARD_SIZE; i++)
        BOARD[i] = Finish[i];
    FINISH = HashFunc();          // 格納場所を求める
    BucketIn(NULL, FINISH, 5);    // ハッシュ表に登録

    // キューにデータがある限りループ
    while (Qhead < Qtail) {
        address1 = QUEUE[Qhead++];   // キュー先頭より取る

        // address1に保存されている局面をBOARD上に再現する
        for (i=0; i<BOARD_SIZE; i++)
            BOARD[i] = address1->board[i];
        space = address1->space;         // 空きマスの座標

        // 可能な次の手を総て実行する
        for (i=0; Moval[space][i]!=-1; i++) {  // 番兵まで
            piece = Moval[space][i]; // 移動させる駒の座標
            n     = BOARD[piece];    // 移動させる駒の数字

            // 局面を生成する
            BOARD[piece] = 0;
            BOARD[space] = n;

            // 始めての局面ならハッシュ表とキューに追加する
            address2 = HashFunc();
            if (address2->space == EMPTY) {
                BucketIn(address1, address2, piece); // ハッシュ表へ
                QUEUE[Qtail++] = address2;           // キューへ
            }

            // 解を発見したか確認(FINISHは終了局面の番地)
            if (address2 == FINISH) {
                FINISH->prev = address1; // 親局面ポインタを記録
                return TRUE; // 解を発見しました
            }

            // 局面を元に戻す
            BOARD[space] = 0;
            BOARD[piece] = n;
        }
    }
    return FALSE; // 解がありません
}
int main(void)
{
    // 幅優先探索を行い解があれば手順を表示
    if (BreadthFirst())
        Display(FINISH);  // 解答手順を表示
    else
        printf("解がありません\n");
    return 0;
}
