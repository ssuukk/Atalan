#include "language.h"

void HeapInit(MemHeap * heap)
{
	heap->capacity = 0;
	heap->count    = 0;
	heap->block    = NULL;
}

void HeapCleanup(MemHeap * heap)
{
	MemFree(heap->block);
}

void HeapRemoveBlock(MemHeap * heap, UInt32 adr, UInt32 size)
{
	MemBlock * mbl;
	UInt32 end, hend;
	UInt32 cnt;

	end = adr + size;
	mbl = heap->block;

	for (cnt = heap->count; size > 0 && cnt > 0; cnt--, mbl++) {
		hend = mbl->adr + mbl->size;

		if (adr >= hend || end <= mbl->adr) continue;

		// mbl->adr.................hend
		//           adr......end
		// Block is split into two (removed block is in the middle)
		if (mbl->adr < adr && hend > end) {
			mbl->size = adr - mbl->adr;
			HeapAddBlock(heap, end, hend - end);
			return;		// there can not be another block to remove and our variables are not correct anyway

		//     mbl->adr...hend
		// adr....................end
		} else if (adr <= mbl->adr && end >= hend) {
			heap->count--;
			memcpy(mbl, mbl+1, sizeof(MemBlock) * (cnt-1));
			mbl--;

		// mbl->adr........hend
		//          adr............end
		} else if (mbl->adr <= adr && hend <= end) {
			mbl->size = adr - mbl->adr;

		//       mbl->adr........hend
		//  adr............end
		} else if (adr <= mbl->adr && end <= hend) {
			mbl->adr = end;
			mbl->size = hend - end;
		}
	}
}

void HeapAddBlock(MemHeap * heap, UInt32 adr, UInt32 size)
/*
Purpose:
	Add new block to heap.
	Nothing is added if such block already exists.
	Blocks will be merged.
*/
{
	MemBlock * mbl;
	UInt32 end, hend;
	UInt32 cnt;

	// Try to append the block to some existing block
	// We may even merge three blocks together

	mbl = heap->block;
	end = adr + size;
	for (cnt = heap->count; size > 0 && cnt > 0; cnt--) {
		hend = mbl->adr + mbl->size;

		// mbl->adr....hend
		//          adr........end
		if (adr >= mbl->adr && adr <= hend) {
			if (end > hend) {
				mbl->size = end - mbl->adr;
			}
			return;
		}
		mbl++;
	}

	if (heap->count == heap->capacity) {
		cnt = (heap->capacity==0)?4:heap->capacity*2;
		heap->block = (MemBlock *)realloc(heap->block, sizeof(MemBlock) * cnt);
		heap->capacity = cnt;
	}

	mbl = &heap->block[heap->count];
	mbl->adr = adr;
	mbl->size = size;
	heap->count++;
}

Bool HeapAllocBlock(MemHeap * heap, UInt32 size, UInt32 * p_adr)
/*
Purpose:
	Alloc block of specified size from current heap.
*/
{
	UInt32 cnt;
	MemBlock * best_mbl, * mbl;
	Bool found = false;

	// We try to find smallest block bigger or equal to requested size

	best_mbl = NULL;
	mbl = heap->block;
	for (cnt = heap->count; size > 0 && cnt > 0; cnt--) {
		if (mbl->size >= size && (best_mbl == NULL || best_mbl->size > mbl->size)) {
			best_mbl = mbl;
			if (mbl->size == size) break;		// if size of the block is same as requested, we do not need to search further
		}
		mbl++;
	}

	if (best_mbl != NULL) {
		*p_adr = best_mbl->adr;
		best_mbl->adr += size;
		best_mbl->size -= size;
		// Remove block, if it is empty
		if (best_mbl->size == 0) {
			mbl = &heap->block[heap->count - 1];
			if (mbl != best_mbl) {
				best_mbl->adr  = mbl->adr;
				best_mbl->size = mbl->size;
			}
			heap->count--;
		}
		found = true;
	}


	return found;
}

void HeapAddType(MemHeap * heap, Type * type)
/*
Purpose:
	Add to heap list of blocks defined by type.
	Type must be integer type defining a range or variant type which has two subtypes.
*/
{
	BigInt sz;
	if (type != NULL) {
		if (type->variant == TYPE_INT) {
			IntRangeSize(&sz, &type->range.min, &type->range.max);
			HeapAddBlock(heap, IntN(&type->range.min), IntN(&sz));
			IntFree(&sz);
		} else if (type->variant == TYPE_VARIANT) {
			HeapAddType(heap, type->left);
			HeapAddType(heap, type->right);
		}
	}
}

void HeapPrint(MemHeap * heap)
{
	UInt32 cnt;
	MemBlock * mbl;
	Print("--------------\n");
	for(cnt = heap->count, mbl = heap->block; cnt > 0; cnt--, mbl++) {
		PrintFmt("%d-%d (%d)\n", mbl->adr, mbl->adr+mbl->size-1, mbl->size);
	}
}

void HeapUnitTest()
/*
Purpose:
	Procedure used to test the heap function in debug mode.
*/
{
	MemHeap heap;

	HeapInit(&heap);

	HeapAddBlock(&heap, 128, 64);
	HeapPrint(&heap);
	HeapAddBlock(&heap, 128+64, 64);
	HeapPrint(&heap);

	// Removing from the beginning
	HeapRemoveBlock(&heap, 128, 2);
	HeapPrint(&heap);
	// Removing from the end
	HeapRemoveBlock(&heap, 128, 2);
	HeapPrint(&heap);
	// Removing from the end
	HeapRemoveBlock(&heap, 254, 2);
	HeapPrint(&heap);

	// Removing from the end (bigger chunk)
	HeapRemoveBlock(&heap, 253, 15);
	HeapPrint(&heap);

	// Removing from middle

	HeapRemoveBlock(&heap, 136, 2);
	HeapPrint(&heap);

	// Removing complete block & some more

	HeapRemoveBlock(&heap, 128, 32);
	HeapPrint(&heap);

	HeapCleanup(&heap);

}
