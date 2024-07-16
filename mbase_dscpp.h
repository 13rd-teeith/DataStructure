#pragma once
#include "iostream"
#include <string>
#define _TREE_EMPTY_CHAR_     35			//encoded characters for empty tree nodes (most for huffman tree)
#define _TREE_DEF_VALUE_	  0			    //value for empty tree nodes
#define _ARRAY_MAX_SIZE_      0xFFFFFF		//maximum settable size of an array to 2^24(0xFFFFFF).
#define _ULL_BITLEN_          0x40			//the bit length of an unsigned long long is 64 bits (0x40).
#define _GROWTH_FACTOR_		  2		   	    //the growth factor for array
#define _HEAP_DEF_CAPACITY_   128				//heap default capacity

using Byte          =  unsigned char;	    //self defined Byte type.
using HuffmanValue  =  int;				    //huffman tree value(weight) type.
using HuffmanChar   =  char;			    //huffman tree encoded character type.

//Array
template<typename VT>
class Array {
private:
	size_t capacity;
	VT* container;
	void copyFrom(const Array<VT>& other) {
		this->capacity = other.capacity;
		this->container = new VT[this->capacity];
		for (size_t i = 0; i < this->capacity; ++i)
			this->container[i] = other.container[i];
	}
public:
	Array(const Array<VT>& other) { copyFrom(other); }
	Array(size_t initSize = 0) :capacity(initSize), container(new VT[initSize]) {}
	Array(size_t initSize, const VT& defaultValue) :capacity(initSize),
		container(new VT[initSize]) {
		for (size_t i = 0; i < initSize; ++i) container[i] = defaultValue;
	}

	~Array() { delete[] container; }

	void expand() {
		capacity *= _GROWTH_FACTOR_;
		VT* newContainer = new VT[capacity];
		for (size_t i = 0; i < capacity; ++i)
			newContainer[i] = container[i];
		delete[] container;
		container = newContainer;
	}
	size_t getSize() const{
		return capacity;
	}	
	VT& operator[](const size_t index) const{ 
		if (index > capacity )
			throw std::out_of_range("Out of index range.");
		else return container[index];
	}
	
	Array<VT>& operator=(const Array<VT>& other) {
		if (this != &other) { // self copy check.
			delete[] this->container; 
			copyFrom(other); 
		}
		return *this;
	}
};

/*
* Flag Description: 
*	/////////////////////////////////////////////////////////////////////
*	// The Flag class provides a mechanism for managing a large number //
*	// of boolean flags using bit operations.   Flags are stored within//
*	// one or more 'size_t' integers, where each bit represents a flag.//
*	/////////////////////////////////////////////////////////////////////
*/
class Flag {
private:
	size_t* flag;
	size_t flagNumber, capacity;
public:
	Flag(const size_t flagNumber) :flag(0), flagNumber(flagNumber) {
		if (flagNumber > _ARRAY_MAX_SIZE_)
			throw std::out_of_range("Unable to handle so many tags.");
		else {
			capacity = 1 + _ARRAY_MAX_SIZE_ / _ULL_BITLEN_;
			flag = new size_t[capacity];
			clear();
		}
	}
	~Flag() { delete[] flag; }

	Flag(const Flag&) = delete;
	Flag& operator=(const Flag&) = delete;

	void mark(const unsigned int index) {
		if (index > capacity) 
			throw std::out_of_range("Unable to access this position: [index].");  

		unsigned int blockIndex = index / _ULL_BITLEN_;
		unsigned int innerIndex = index % _ULL_BITLEN_; 
		flag[blockIndex] |= (1ULL << innerIndex);
	}

	void revokeMark(const unsigned int index) {
		if (index > capacity) 
			throw std::out_of_range("Unable to access this position: [index]."); 

		unsigned int blockIndex = index / _ULL_BITLEN_; 
		unsigned int innerIndex = index % _ULL_BITLEN_; 
		flag[blockIndex] &= ~(1ULL << innerIndex);
	}

	bool isMarked(const unsigned int index) const {
		if (index > capacity) 
			throw std::out_of_range("Unable to access this position: [index]."); 

		unsigned int blockIndex = index / _ULL_BITLEN_; 
		unsigned int innerIndex = index % _ULL_BITLEN_; 
		return (flag[blockIndex] & (1ULL << innerIndex)) != 0;
	}

	void clear() {
		for (unsigned int i = 0; i < capacity; ++i) flag[i] = 0;
	}
};

//Stack
template<typename VT> 
class Stack {
private:
	size_t currentSize;
	const size_t capacity; 
	VT* container;
public:
	Stack(size_t capacity) : currentSize(0), capacity(capacity), container(new VT[capacity]) {}
	~Stack() { delete[] container; }
	Stack(const Stack&) = delete;
	Stack& operator=(const Stack&) = delete; 

	void push(const VT& value) {
		if (isFull()) 
			throw std::out_of_range("Unable to push data to full stack.");
		else {
			container[currentSize] = value; 
			++currentSize; 
		}
	}

	void pop() {
		if (isEmpty()) 
			throw std::underflow_error("Unable to pop from an empty stack.");
		else --currentSize; 
	}

	const VT& top() const{
		if (isEmpty())  
			throw std::underflow_error("Unable to access the top of an empty stack.");
		return container[currentSize - 1]; 
	} 

	bool isEmpty() const {
		return currentSize == 0;
	}

	bool isFull() const {
		return currentSize == capacity;
	}

	size_t getSize() const {
		return currentSize;
	}
};


//Queue(Circular Queue)
template <typename VT>
class Queue {
private:
    VT* container;
    const size_t capacity;
	size_t frontPointer, rearPointer, currentSize;

public:
    Queue(size_t capacity) : capacity(capacity), frontPointer(0), rearPointer(0), currentSize(0), container(new VT[capacity]) {}
    ~Queue() { delete[] container; }
    Queue(const Queue&) = delete;
    Queue& operator=(const Queue&) = delete;

    void push(const VT& value) {
        if (isFull()) {
            throw std::out_of_range("Unable to push data to full queue.");
        }
        container[rearPointer] = value;
        rearPointer = (rearPointer + 1) % capacity;
        currentSize++;
    }

    void pop() {
        if (isEmpty()) {
            throw std::underflow_error("Unable to access the front of an empty queue.");
        }
        VT result = container[frontPointer];
        frontPointer = (frontPointer + 1) % capacity;
        --currentSize;
    }

    VT front() const {
        if (isEmpty()) {
            throw std::underflow_error("Unable to access the front of an empty queue.");
        }
        return container[frontPointer];
    }

    bool isEmpty() const {
        return currentSize == 0;
    }

    bool isFull() const {
        return currentSize == capacity;
    }

    size_t getSize() const { 
        return currentSize;
    }
};

//RCMatrix(Triplet Compressed Matrix)
class RCMatrix {
private:
public:
};

//CRS(Compressed Row Storage, also known as 'Row-wise Linked Sequential Table') 
template <typename VT>
class CRS {
private:
public:
};

//BiTreeNode(Binary Tree Node)
template<typename VT> 
class BiTreeNode {
public: 
	VT value;
	BiTreeNode<VT>* lChild, * rChild;
	BiTreeNode(const VT& val): value(val), lChild(nullptr), rChild(nullptr) {}
	~BiTreeNode() {}
	static void deleteTree(BiTreeNode<VT>* root) {
		if (!root) return;
		deleteTree(root->lChild);
		deleteTree(root->rChild);
		delete root;
	}
}; 

//PBiTreeNode(Binary Tree Node with Parent Pointer)
template<typename VT>
class PBiTreeNode: public BiTreeNode<VT> {
public:
	PBiTreeNode<VT>* parent;
	PBiTreeNode(VT val): BiTreeNode<VT>(val), parent(nullptr){}
}; 

//ABiTree(Array-based Binary Tree)
template<typename NodeType>
class ABiTree {
private:
	NodeType* container;
	size_t currentSize;
	const size_t capacity;
	Flag nodeFlag;

	bool isRangeOut(const size_t index) const {
		return index > currentSize;
	}

public:
	const size_t rootIndex = 1;

	ABiTree(size_t capacity) : currentSize(0), capacity(capacity),
		container(new NodeType[capacity + 1]), nodeFlag(Flag(capacity)){}

	bool isExist(const size_t index) const{ 
		return index <= currentSize && nodeFlag.isMarked(index) && index > 0;
	}

	size_t getLeftChild(const size_t rootIndex) const{
		return rootIndex * 2;
	}

	size_t getRightChildIndex(const size_t rootIndex) const{ 
		return rootIndex * 2 + 1;
	}

	void setValue(const size_t index, const NodeType& value) {
		if (isRangeOut(index))
			throw std::out_of_range("Index out of range.");
		else {
			container[index] = value;
			nodeFlag.mark(index);
		}
	}

	const NodeType& getValue(const size_t index) {
		if (!isExist(index))
			throw std::out_of_range("This node doesn't exist.");
		else return container[index];
	}
};

//Binary Heap
template<typename VT>
class Heap {
private:
	VT* container;
	size_t currentSize;
	const size_t capacity;

	void swap(const size_t& ind1, const size_t& ind2) {
		VT value = container[ind1];
		container[ind1] = container[ind2];
		container[ind2] = value;
	}
	void upper(size_t i) {
		while (i / 2 > 0) {
			if (container[i] > container[i / 2]) {
				swap(i, i / 2);
				i /= 2;
			}
			else break;
		}
	}
	void bottom(size_t i) {
		while (true) {
			if (2 * i > currentSize) return;
			if (2 * i + 1 > currentSize || container[2 * i] > container[2 * i + 1]) {
				if (container[2 * i] > container[i]) {
					swap(i, 2 * i);
					i *= 2;
				}
				else return;
			}
			else {
				if (container[2 * i + 1] > container[i]) {
					swap(i, 2 * i + 1);
					i = 2 * i + 1;
				}
				else return;
			}
		}
	}
public:

	//Empty Heap
	Heap(const size_t& capacity) :currentSize(0), capacity(capacity) {
		if (capacity > _ARRAY_MAX_SIZE_)
			throw std::out_of_range("Unable to handle so many items.");
		else container = new VT[capacity + 1];
	}

	//Sequenced Heap
	Heap(const size_t& capacity, VT data[], const size_t& dataSize) :currentSize(dataSize), capacity(capacity) {
		if (capacity > _ARRAY_MAX_SIZE_)
			throw std::out_of_range("Unable to handle so many items.");
		else {
			container = new VT[capacity + 1];
			for (size_t i = 1; i <= currentSize; ++i) {
				container[i] = data[i - 1];
			}
			for (size_t i = currentSize / 2; i > 0; --i) {
				bottom(i);
			}
		}
	}
	~Heap() { delete[] container; }

	void push(const VT& value) {
		if (currentSize == capacity)
			throw std::out_of_range("Unable to push data into the full heap.");
		++currentSize;
		container[currentSize] = value;
		upper(currentSize);
	}

	void pop() {
		if (isEmpty())
			throw std::underflow_error("Unable to access the top of the heap.");
		else {
			container[1] = container
				[currentSize];
			--currentSize;
			bottom(1);
		}
	}
	VT top() const {
		if (isEmpty())
			throw std::underflow_error("Unable to access the top of the heap.");
		else return container[1];
	}

	bool isEmpty() const {
		return currentSize == 0;
	}

	size_t getSize() const {
		return currentSize;
	}
};

//Auto Heap
template<typename VT>
class AHeap {
private:
	Array<VT> container;
	size_t currentSize;

	void swap(const size_t& ind1, const size_t& ind2) {
		VT value = container[ind1];
		container[ind1] = container[ind2];
		container[ind2] = value;
	}
	void upper(size_t i) {
		while (i / 2 > 0) {
			if (container[i] > container[i / 2]) {
				swap(i, i / 2);
				i /= 2;
			}
			else break;
		}
	}
	void bottom(size_t i) {
		while (true) {
			if (2 * i > currentSize) return;
			if (2 * i + 1 > currentSize || container[2 * i] > container[2 * i + 1]) {
				if (container[2 * i] > container[i]) {
					swap(i, 2 * i);
					i *= 2;
				}
				else return;
			}
			else {
				if (container[2 * i + 1] > container[i]) {
					swap(i, 2 * i + 1);
					i = 2 * i + 1;
				}
				else return;
			}
		}
	}

	void seqlize() {
		for (size_t i = currentSize / 2; i > 0; --i) 
			bottom(i);
	}
public:
	//Empty Heap
	AHeap() :currentSize(0), container(Array<VT>(_HEAP_DEF_CAPACITY_)) {}

	//Sequenced Heap 
	AHeap(Array<VT> data) :currentSize(data.getSize()), 
		container(Array<VT>(data.getSize() > 0 ? data.getSize() + 1 : _HEAP_DEF_CAPACITY_)) {
		for (size_t i = 1; i <= data.getSize(); ++i)
			container[i] = data[i - 1];
		seqlize();
	}

	void push(const VT& value) {
		if (currentSize + 1 == container.getSize()) container.expand();
		++currentSize; 
		container[currentSize] = value;
		upper(currentSize);
	}

	void pop() {
		container[1] = container[currentSize];
		--currentSize;
		bottom(1);
	}
	VT top() const {
		if (isEmpty())
			throw std::underflow_error("Unable to access the top of the heap.");
		else return container[1];
	}

	bool isEmpty() const {
		return currentSize == 0;
	}

	size_t getSize() const {
		return currentSize;
	}
};

//B Tree Node
template<typename VT>
class BTreeNode {
private:
public:
};

//B Plus Tree Node
template<typename VT>
class BPTreeNode {
private:
public:
};

// HTreeNode(HuffmanTree Node)
class HTreeNode {
public:
	HuffmanValue value; 
	BiTreeNode<HuffmanChar>* root;
	friend bool operator<(const HTreeNode& lhs, const HTreeNode& rhs){
		return lhs.value < rhs.value;
	}

	friend bool operator>(const HTreeNode& lhs, const HTreeNode& rhs) {
		return lhs.value > rhs.value;
	}

	friend bool operator==(const HTreeNode& lhs, const HTreeNode& rhs) {
		return lhs.value == rhs.value;
	}

	HTreeNode(HuffmanChar encodedChar = _TREE_EMPTY_CHAR_, HuffmanValue value = _TREE_DEF_VALUE_) :
		value(value), root(new BiTreeNode<HuffmanChar>(encodedChar)) {}
};

//SHuffmanTree(Static Encoding Huffman Tree)
class SEHuffmanTree {
private:
	BiTreeNode<HuffmanChar>* root;
	size_t charSetSize;
public:
	SEHuffmanTree(std::string code, size_t charSetSize, size_t statistics[],HuffmanChar dictionary[]) :charSetSize(charSetSize) {
		HTreeNode* huffmanTree = new HTreeNode[charSetSize];
		for (size_t i = 0; i < charSetSize; ++i) statistics[i] = 0;

		Heap<HTreeNode> heap 
			= Heap<HTreeNode>(charSetSize, huffmanTree, charSetSize); //datasize and capacity is the same here.
		while (heap.getSize() > 1) {
			HTreeNode tree1 = heap.top();
			heap.pop(); 
			HTreeNode tree2 = heap.top();
			heap.pop(); 

			HTreeNode newTree = HTreeNode(_TREE_EMPTY_CHAR_, tree1.value + tree2.value);
			newTree.root->lChild = tree1.root;
			newTree.root->rChild = tree2.root; 
			heap.push(newTree);
		}
		root = heap.top().root;
	}
};

//DHuffmanTree(Dynamic Encoding Huffman Tree)
class DEHuffmanTree {
private:
	BiTreeNode<HuffmanChar>* root;
	size_t charSetSize;
public:
	//encode build.
	DEHuffmanTree(std::string code){

	}

	//decode build
}; 

//AMatGraph(AdjacencyMatrix Graph)
template<typename VT>
class AMatGraph {
private:
public:
};

//ALGraph(AdjacencyList Graph)
template<typename VT>
class ALGraph {
private:
public:
};

//ILGraph(IncidenceList Graph)
template<typename VT>
class ILGraph {
private:
public:
};

//AMulGraph(AdjacencyMultilist Graph)
template<typename VT>
class AMulGraph {
private:
public:
};

/*
* CFSGraph(Chain-like Forward Star Graph) Description:
* ///////////////////////////////////////////////////////////////////
* // The CFSGraph class is intended to represent a graph using a   //
* // chain-like forward star structure, a data structure optimized //
* // for storing graphs with a large number of edges, especially   //
* // useful in sparse graphs.                                      //
* ///////////////////////////////////////////////////////////////////
*/
template<typename VT>
class CFSGraph { 
private:
public:
};

//AVLTree(Adelson-Velsky and Landis Tree)
template<typename VT>
class AVLTree {
private:
public:
};

/*
* RBTree(Red-Black Tree) Description:
* ///////////////////////////////////////////////////////////////////////// 
* // The RBTree class is intended to implement a Red-Black Tree, a type  //
* // of self-balancing binary. It maintains a balanced tree by enforcing //
* // a set of properties, ensuring that the longest path from the root to//
* // a leaf is not much longer than the shortest path, which results in  //
* // efficient search, insert, and delete operations.                    //
* /////////////////////////////////////////////////////////////////////////
*/
template<typename VT> 
class RBTree { 
private:
public:
};