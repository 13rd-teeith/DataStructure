#pragma once

/// ====================
///	==== FOR STUDY  ====
/// ====================

#include "iostream"
#include <string>
#include <map>
#define _NULLSTR_FLAG_		  "NULL"		//null string flag
#define _EMPTY_CHAR_          '\0'			//empty char 
#define _NULL_STRING_         ""			//null string value
#define _TREE_DEF_VALUE_	  0			    //value for empty tree nodes
#define _ARRAY_MAX_SIZE_      0xFFFFFF		//maximum settable size of an array to 2^24(0xFFFFFF).
#define _STR_MAX_SIZE_		  0xFFFFFF		//maximum settable size of an string to 2^24(0xFFFFFF).
#define _ULL_BITLEN_          0x40			//the bit length of an unsigned long long is 64 bits (0x40).
#define _GROWTH_FACTOR_		  2.0		   	//the growth factor for array
#define _HEAP_DEF_CAPACITY_   128			//heap default capacity
#define _0_					  '0'			//huffman code 0
#define _1_					  '1'			//huffman code 1		  

using Byte = unsigned char;	    //self defined Byte type.
using HuffmanValue = size_t;				//huffman tree value(weight) type.
using HuffmanChar = char;			    //huffman tree encoded character type.
using StringChar = char;   		        //char type for class String

//Array
template<typename VT>
class Array {
private:
	size_t capacity;
	VT* container = nullptr;
	void deepCopy(const Array<VT>& other) {
		if(this->container) delete[] this->container;
		this->capacity = other.capacity;
		this->container = new VT[other.capacity];
		for (size_t i = 0; i < this->capacity; ++i)
			this->container[i] = other.container[i];
	}
public:
	Array(const Array<VT>& other) { deepCopy(other); }
	Array(size_t initSize = 0) :capacity(initSize), container(initSize == 0 ? nullptr: new VT[initSize]) {}
	Array(size_t initSize, const VT& defaultValue) :capacity(initSize),
		container(new VT[initSize]) {
		for (size_t i = 0; i < initSize; ++i) container[i] = defaultValue;
	}
	~Array() { delete[] container; }

	void autoExpand(double grouthFactor = _GROWTH_FACTOR_) {
		resize((size_t)(capacity * _GROWTH_FACTOR_));
	}

	void resize(size_t newSize) {
		if (newSize <= capacity)
			throw std::runtime_error("Unnecessary resize");
		else {
			VT* newContainer = new VT[newSize];
			size_t copySize = std::min(capacity, newSize);
			for (size_t i = 0; i < copySize; ++i)
				newContainer[i] = container[i];
			delete[] container;
			container = newContainer;
			capacity = newSize;
		}
	}

	size_t getSize() const {
		return capacity;
	}

	VT& operator[](const size_t index) const {
		if (index >= capacity)
			throw std::out_of_range("Out of index range.");
		else return container[index];
	}

	Array<VT>& operator=(const Array<VT>& other) {
		if (this != &other) deepCopy(other);// self copy check. 
		return *this;
	}

	template<typename VT>
	friend std::ostream& operator<<(std::ostream& os, const Array<VT>& arr);
};

//overload class Array output (most for debug)
template<typename VT>
static std::ostream& operator<<(std::ostream& os, const Array<VT>& arr) {
	size_t size = arr.getSize();
	os << "[";
	for (size_t i = 0; i < size - 1; ++i)
		if (arr[i] == _EMPTY_CHAR_) os << _NULLSTR_FLAG_ << ",";
		else os << arr[i] << ",";
	if (arr[size - 1] == _EMPTY_CHAR_) os << _NULLSTR_FLAG_ << "]";
	else os << arr[size - 1] << "]";
	return os;
}

//String
class String {
private:
	StringChar* container;
	size_t length;
	void deepCopy(const String& string) {
		if (this->container) delete[] this->container;
		this->length = string.length;
		this->container = new StringChar[string.length + 1];
		for (size_t i = 0; i < string.length; ++i)
			this->container[i] = string.container[i];
		this->container[string.length] = _EMPTY_CHAR_;
	}
public:
	String(StringChar strChar) : container(new StringChar[2]), length(1) {
		container[0] = strChar;
		container[1] = _EMPTY_CHAR_;
	}

	String(size_t length = 0) :container(new StringChar[length + 1]), length(length) {
		for (size_t i = 0; i <= length; ++i)
			container[i] = _EMPTY_CHAR_;
	}

	String(const String& string) :container(nullptr) {
		deepCopy(string);
	}

	String(const StringChar string[]) :length(strlen(string)) {
		container = new StringChar[length + 1];
		if (length >= _STR_MAX_SIZE_) throw std::out_of_range("Out of bounds.");
		for (size_t i = 0; i < length; ++i)
			container[i] = string[i];
		container[length] = _EMPTY_CHAR_;
	}

	~String() {
		delete this->container;
		this->container = nullptr;
	}

	size_t getLength() const { return length; };
	void debugOutput() { 
		size_t size = length;
		std::cout << "[";
		for (size_t i = 0; i < size - 1; ++i)
			if (container[i] == _EMPTY_CHAR_) std::cout << _NULLSTR_FLAG_ << ",";
			else std::cout << (size_t)container[i] << ",";
		if (container[size - 1] == _EMPTY_CHAR_) std::cout << _NULLSTR_FLAG_ << "]";
		else std::cout << (size_t)container[size - 1] << "]";
	}

	const String& operator=(const String& other) {
		if (this != &other) // self check 
			deepCopy(other);
		return *this;
	}

	const String& operator=(const StringChar string[]) {
		size_t len = strlen(string);
		if (this->container) delete[] this->container;
		this->length = len;
		this->container = new StringChar[len + 1];
		for (size_t i = 0; i < len; ++i)
			this->container[i] = string[i];
		this->container[len] = _EMPTY_CHAR_;
		return *this;
	}

	String operator+(const String& other) const {
		size_t newLength = this->length + other.length;
		String result(newLength + 1);
		result.length = newLength;
		size_t i;
		for (i = 0; i < this->length; ++i)
			result.container[i] = this->container[i];
		for (size_t j = 0; j < other.length; ++j)
			result.container[i + j] = other.container[j];
		result.container[result.length] = _EMPTY_CHAR_;
		return result;
	}

	String operator+(StringChar string[]) const {
		size_t stringSize = strlen(string);
		if (stringSize >= _STR_MAX_SIZE_) throw std::out_of_range("Out of bounds.");
		size_t oriLength = this->length;
		size_t newLength = +stringSize;
		String result(newLength + 1);
		result.length = newLength;
		for (size_t i = 0; i < oriLength; ++i)
			result.container[i] = this->container[i];
		for (size_t j = 0; j < stringSize; ++j)
			result.container[oriLength + j] = string[j];
		result.container[newLength] = _EMPTY_CHAR_;
		return result;
	}

	String& operator+=(const String& other) {
		*this = *this + other;
		return (*this);
	}

	String& operator+=(StringChar string[]) {
		*this = *this + string;
		return (*this);
	}

	friend std::ostream& operator<<(std::ostream& os, const String& string);

	StringChar& operator[](const size_t index) const {
		if (index > length)
			throw std::out_of_range("Out of index range.");
		else return container[index];
	}

	bool operator==(const String& other) const {
		if (this->length != other.length)
			return false;
		/*
		* Internal access should be via container,
		* and using overload op"[]" directly will
		* increase the cost of security validation.
		*/
		for (size_t i = 0; i < this->length; ++i)
			if (this->container[i] != other.container[i])
				return false;
		return true;
	}
	bool operator==(const StringChar string[]) const {
		size_t length = strlen(string);
		if (length >= _STR_MAX_SIZE_) throw std::out_of_range("Out of bounds.");
		if (this->length != length) return false;
		for (size_t i = 0; i < this->length; ++i)
			if (this->container[i] != string[i])
				return false;
		return true;
	}
	bool operator!=(const String& other) const {
		return !(*this == other);
	}
	bool operator!=(const StringChar string[]) const {
		return !(*this == string);
	}
};
//overload class String output
static std::ostream& operator<<(std::ostream& os, const String& string) {
	size_t length = string.length;
	for (size_t i = 0; i < length; ++i)
		os << string.container[i];
	return os;
}

/* === English character set only[BEGIN] === */
static const StringChar		ECSO_NULL = '$';
static const size_t			ECSO_INVI = 0;       //Invalid Index
static const size_t			ECSO_SIZE = 86;
static const StringChar		ECSO_CHAR[] =
{
	_EMPTY_CHAR_,
	'a', 'b', 'c', 'd', 'e', 'f', 'g',
	'h', 'i', 'j', 'k', 'l', 'm', 'n',
	'o', 'p', 'q', 'r', 's', 't', 'u',
	'v', 'w', 'x', 'y', 'z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G',
	'H', 'I', 'J', 'K', 'L', 'M', 'N',
	'O', 'P', 'Q', 'R', 'S', 'T', 'U',
	'V', 'W', 'X', 'Y', 'Z',
	'!', '\"','#', '$', '%', '&', '\'',
	'(', ')', '*', '+', ',', '-', '.',
	'/', ':', ';', '<', '=', '>', '?',
	'@', '[', '\\',']', '^', '_', '`',
	'{', '|', '}', '~', ' '
};
static const size_t			ECSO_STA[] =
{
	0,
	800 , 150 , 300 , 450 , 1300, 250 , 200 ,	// a-g
	650 , 750 , 50  , 50  , 450 , 250 , 700 ,	// h-l
	700 , 200 , 50  , 600 , 650 , 950 , 300 ,	// m-r
	100 , 200 , 50  , 200 , 50  , 				// s-z
	80  , 15  , 30  , 45  , 130 , 25  , 20  ,	// A-G
	65  , 75  , 5   , 5   , 45  , 25  , 70  ,	// H-L
	70  , 20  , 5   , 60  , 65  , 95  , 30  ,	// M-R
	10  , 20  , 5   , 20  , 5   ,				// S-Z
	5   , 10  , 1   , 1   , 1   , 2   , 10  ,	// !-"'
	10  , 10  , 1   , 2   , 200 , 100 , 150 ,	// (,-.
	5   , 10  , 5   , 1   , 5   , 1   , 5   ,	// /-?
	1   , 1   , 1   , 1   , 1   , 5   , 1   ,	// @-\]
	1   , 1   , 1   , 1   , 3500				// ^-~
};
static const String			ECSO_NAME = "ECSO_NAME";
static const size_t			ECSO_MAPSIZE = 128;
static const size_t			ECSO_MAP[] =
{
	ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,
	ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,
	ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,
	ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,
	85,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
	ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,ECSO_INVI,
	ECSO_INVI,ECSO_INVI,
	68,69,70,71,72,73,74,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,
	47,48,49,50,51,52,75,76,77,78,79,80,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
	19,20,21,22,23,24,25,26,81,82,83,84
};
/* === English character set only[ END ] === */




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

	const VT& top() const {
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
	BiTreeNode(const VT& val) : value(val), lChild(nullptr), rChild(nullptr) {}
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
class PBiTreeNode : public BiTreeNode<VT> {
public:
	PBiTreeNode<VT>* parent;
	PBiTreeNode(VT val) : BiTreeNode<VT>(val), parent(nullptr) {}
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
		container(new NodeType[capacity + 1]), nodeFlag(Flag(capacity)) {}

	bool isExist(const size_t index) const {
		return index <= currentSize && nodeFlag.isMarked(index) && index > 0;
	}

	size_t getLeftChild(const size_t rootIndex) const {
		return rootIndex * 2;
	}

	size_t getRightChildIndex(const size_t rootIndex) const {
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
	Heap(const Heap&) = delete;
	Heap& operator=(const Heap&) = delete;
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
	AHeap& operator=(const AHeap&) = delete;

	void push(const VT& value) {
		if (currentSize + 1 == container.getSize()) container.autoExpand();
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
	BiTreeNode<String>* root;
	friend bool operator<(const HTreeNode& lhs, const HTreeNode& rhs) {
		return lhs.value > rhs.value;
	}

	friend bool operator>(const HTreeNode& lhs, const HTreeNode& rhs) {
		return lhs.value < rhs.value;
	}

	friend bool operator==(const HTreeNode& lhs, const HTreeNode& rhs) {
		return lhs.value == rhs.value;
	}

	HTreeNode(const String& encodedChar = _NULL_STRING_, const HuffmanValue& value = _TREE_DEF_VALUE_) :
		value(value), root(new BiTreeNode<String>(encodedChar)) {}
};
// Huffman Dictionary
class HuffmanDictionary {
private:
	size_t charsetSize;
	Array<size_t> statistic;
	Array<size_t> charMap;
	Array<StringChar> dictionary;
	String name;
public:
	HuffmanDictionary(String name, size_t charsetSize, size_t mapSize,
		size_t sta[], StringChar dic[], size_t map[]) :
		charsetSize(charsetSize), statistic(Array<size_t>(charsetSize)),
		dictionary(Array<StringChar>(charsetSize)), charMap(Array<size_t>(mapSize)) {
		for (size_t i = 0; i < charsetSize; ++i) {
			statistic[i] = sta[i];
			dictionary[i] = dic[i];
		}
		for (size_t i = 0; i < mapSize; ++i) charMap[i] = map[i];
	}

	HuffmanDictionary(const String name, const size_t charsetSize, const size_t mapSize,
		const size_t sta[], const StringChar dic[], const size_t map[]) :
		charsetSize(charsetSize), statistic(Array<size_t>(charsetSize)),
		dictionary(Array<StringChar>(charsetSize)), charMap(Array<size_t>(mapSize)) {
		for (size_t i = 0; i < charsetSize; ++i) {
			statistic[i] = sta[i];
			dictionary[i] = dic[i];
		}
		for (size_t i = 0; i < mapSize; ++i) charMap[i] = map[i];
	}

	size_t getCharsetSize() { return charsetSize; }
	StringChar getChar(const size_t index) const {
		if (index >= charsetSize) throw std::out_of_range("Unable to access out-of-bounds value.");
		else return dictionary[index];
	}
	size_t getWeight(const size_t index) const {
		if (index >= charsetSize) throw std::out_of_range("Unable to access out-of-bounds value.");
		else return statistic[index];
	}
	size_t getMapIndex(const StringChar charIndex) const {
		if (charIndex >= charMap.getSize()) throw std::out_of_range("Unable to access out-of-bounds value.");
		else return charMap[charIndex];
	}
};
//English character set only
static HuffmanDictionary HUFF_ECSO =
HuffmanDictionary(ECSO_NAME, ECSO_SIZE, ECSO_MAPSIZE, ECSO_STA, ECSO_CHAR, ECSO_MAP);
//SHuffmanTree(Static Encoding Huffman Tree)
class SEHuffmanTree {
private:
	BiTreeNode<String>* root;
	Array<String> codeList;
	HuffmanDictionary dictionary;
	void buildCodeTree(BiTreeNode<String>* root, String pathCode) {
		//Check thoroughly
		if (!root) {
			std::cout << "May some potential risks here." << std::endl;
			return;
		}

		if (root->value == _NULL_STRING_) {
			buildCodeTree(root->lChild, pathCode + "0");
			buildCodeTree(root->rChild, pathCode + "1");
		}
		else {
			//Check thoroughly
			if (root->lChild || root->rChild || root->value.getLength() != 1)
				std::cout << "May some potential risks here." << std::endl;
			size_t index = dictionary.getMapIndex(root->value[0]);
			codeList[index] = pathCode;
		}
	}
public:
	SEHuffmanTree(HuffmanDictionary dictionary = HUFF_ECSO) :dictionary(dictionary) {
		Array<HTreeNode> huffmanTree = Array<HTreeNode>(dictionary.getCharsetSize());
		codeList = Array<String>(dictionary.getCharsetSize());

		for (size_t i = 0; i < dictionary.getCharsetSize(); ++i)
			huffmanTree[i] = HTreeNode(dictionary.getChar(i), dictionary.getWeight(i));

		AHeap<HTreeNode> priorityQueue = AHeap<HTreeNode>(huffmanTree);
		while (priorityQueue.getSize() > 1) {
			HTreeNode tree1 = priorityQueue.top();
			priorityQueue.pop();
			HTreeNode tree2 = priorityQueue.top();
			priorityQueue.pop();

			HTreeNode newTree = HTreeNode(_NULL_STRING_, tree1.value + tree2.value);
			newTree.root->lChild = tree1.root;
			newTree.root->rChild = tree2.root;
			priorityQueue.push(newTree);
		}
		root = priorityQueue.top().root;
		buildCodeTree(root, _NULL_STRING_);

		for (size_t i = 0; i < codeList.getSize(); ++i) {
			std::cout << dictionary.getChar(i) << ":" << codeList[i] << std::endl;
		}
	}
	~SEHuffmanTree() { BiTreeNode<String>::deleteTree(this->root); }

	String encode(const String& string) {
		String result = _NULL_STRING_;
		size_t index;
		for (size_t i = 0; i < string.getLength(); ++i) {
			index = dictionary.getMapIndex(string[i]);
			result += codeList[index];
		}
		return result;
	}

	String decode(const String& string) {
		String result = _NULL_STRING_;
		BiTreeNode<String>* pr = root;
		for (size_t i = 0; i <= string.getLength(); ++i) {
			if (!pr) throw std::runtime_error("Unknown error.");
			else {
				if (!pr->lChild && !pr->rChild) {
					result += pr->value;
					pr = root;
				}
				if (string[i] == _0_)
					pr = pr->lChild;
				else if (string[i] == _1_)
					pr = pr->rChild;
			}
		}
		return result;
	}

};
//DHuffmanTree(Dynamic Encoding Huffman Tree)
class DEHuffmanTree {
private:
	BiTreeNode<HuffmanChar>* root;
	size_t charSetSize;
public:
	//encode build.
	DEHuffmanTree(std::string code) {

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