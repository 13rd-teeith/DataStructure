#pragma once
#include "iostream"
/*
* Limiting the maximum settable size of an array to 2^24(0xFFFFFF).
* Modifying this value according to the actual situation.
*/
#define _ARRAY_MAX_SIZE_ 0xFFFFFF

/*
* Assuming that the bit length of an unsigned long long is 64 bits (0x40).
* The actual value may vary on different platforms.
*/
#define _ULL_BITLEN_ 0x40  

/*
* Self defined Byte type.
*/
using Byte = unsigned char;


/*
* Flag Description: 
/////////////////////////////////////////////////////////////////////
// The Flag class provides a mechanism for managing a large number //
// of boolean flags using bit operations.   Flags are stored within//
// one or more 'size_t' integers, where each bit represents a flag.//
/////////////////////////////////////////////////////////////////////
*/
class Flag {
private:
	size_t* flag;
	unsigned int flagNumber, capacity;
public:
	Flag(const unsigned int flagNumber) :flag(0), flagNumber(flagNumber) {
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
template<typename ValueType> 
class Stack {
private:
	size_t currentSize;
	const size_t capacity; 
	ValueType* container;
public:
	Stack(size_t capacity) : currentSize(0), capacity(capacity), container(new ValueType[capacity]) {}
	~Stack() { delete[] container; }
	Stack(const Stack&) = delete;
	Stack& operator=(const Stack&) = delete; 

	void push(const ValueType& value) {
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

	const ValueType& top() const{
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
template <typename ValueType>
class Queue {
private:
    ValueType* container;
    const size_t capacity;
	size_t frontPointer, rearPointer, currentSize;

public:
    Queue(size_t capacity) : capacity(capacity), frontPointer(0), rearPointer(0), currentSize(0), container(new ValueType[capacity]) {}
    ~Queue() { delete[] container; }
    Queue(const Queue&) = delete;
    Queue& operator=(const Queue&) = delete;

    void push(const ValueType& value) {
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
        ValueType result = container[frontPointer];
        frontPointer = (frontPointer + 1) % capacity;
        --currentSize;
    }

    ValueType front() const {
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
template <typename ValueType>
class CRS {
private:
public:
};

//BiTreeNode(Binary Tree Node)
template<typename ValueType> 
class BiTreeNode {
public: 
	ValueType value;
	BiTreeNode<ValueType>* lChild, * rChild;
	BiTreeNode(const ValueType& val)
		: value(val), lChild(nullptr), rChild(nullptr) {}
	~BiTreeNode() {}
	static void deleteTree(BiTreeNode<ValueType>* root) {
		if (!root) return;
		deleteTree(root->lChild);
		deleteTree(root->rChild);
		delete root;
	}
}; 

//PBiTreeNode(Binary Tree Node with Parent Pointer)
template<typename ValueType>
class PBiTreeNode: public BiTreeNode<ValueType> {
public:
	PBiTreeNode<ValueType>* parent;
	PBiTreeNode(ValueType val): BiTreeNode<ValueType>(val), parent(nullptr){}
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

//BinaryHeap
template<typename ValueType>
class Heap{ 
private:
	ValueType* container;
	size_t currentSize; 
	const size_t capacity;

	void swap(const size_t& ind1, const size_t& ind2) {
		ValueType value = container[ind1];
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
	Heap(const size_t& capacity) :currentSize(0), capacity(capacity){
		if (capacity > _ARRAY_MAX_SIZE_)
			throw std::out_of_range("Unable to handle so many items."); 
		else container = new ValueType[capacity + 1];
	}

	//Sequenced Heap
	Heap(const size_t& capacity, ValueType data[], const size_t& dataSize) :currentSize(dataSize), capacity(capacity) {
		if (capacity > _ARRAY_MAX_SIZE_)
			throw std::out_of_range("Unable to handle so many items.");
		else {
			container = new ValueType[capacity + 1];
			for (size_t i = 1; i <= currentSize; ++i) {
				container[i] = data[i - 1];
			}
			for (size_t i = currentSize / 2; i > 0; --i) {
				bottom(i);
			}
		}
	}
	~Heap() { delete[] container; }

	void push(const ValueType& value) {
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
	ValueType top() const {
		if (isEmpty())
			throw std::underflow_error("Unable to access the top of the heap.");
		else return container[1];
	}

	bool isEmpty() const{
		return currentSize == 0;
	}

	size_t getSize() const{
		return currentSize; 
	}
};

//B Tree Node
template<typename ValueType>
class BTreeNode {
private:
public:
};

//B Plus Tree Node
template<typename ValueType>
class BPTreeNode {
private:
public:
};

// HTreeNode(HuffmanTree Node)
class HTreeNode {
private:
public:
};

//Huffman Tree and Huffman coding
class HuffmanTree {
private:
	HTreeNode* root; 
public:

};

//AMatGraph(AdjacencyMatrix Graph)
template<typename ValueType>
class AMatGraph {
private:
public:
};

//ALGraph(AdjacencyList Graph)
template<typename ValueType>
class ALGraph {
private:
public:
};

//ILGraph(IncidenceList Graph)
template<typename ValueType>
class ILGraph {
private:
public:
};

//AMulGraph(AdjacencyMultilist Graph)
template<typename ValueType>
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
template<typename ValueType>
class CFSGraph { 
private:
public:
};

//AVLTree(Adelson-Velsky and Landis Tree)
template<typename ValueType>
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
template<typename ValueType> 
class RBTree { 
private:
public:
};