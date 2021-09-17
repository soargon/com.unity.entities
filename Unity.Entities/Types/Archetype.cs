using System;
using System.Runtime.InteropServices;
using Unity.Assertions;
using Unity.Collections.LowLevel.Unsafe;

namespace Unity.Entities
{
    [Flags]
    internal enum ArchetypeFlags : ushort
    {
        SystemStateCleanupComplete = 1,
        SystemStateCleanupNeeded = 2,
        Disabled = 4,
        Prefab = 8,
        HasChunkHeader = 16,
        HasBlobAssetRefs = 32,
        HasCompanionComponents = 64,
        HasBufferComponents = 128,
        HasManagedComponents = 256,
        HasManagedEntityRefs = 512,
        HasWeakAssetRefs = 1024
    }

    [StructLayout(LayoutKind.Sequential)]
    internal unsafe struct Archetype
    {
        public ArchetypeChunkData Chunks;
        public UnsafePtrList<Chunk> ChunksWithEmptySlots;

        public ChunkListMap FreeChunksBySharedComponents;
        public ComponentTypeInArchetype* Types;
        public int* EnableableTypeIndexInArchetype;

        // back pointer to EntityQueryData(s), used for chunk list caching
        public UnsafeList<IntPtr> MatchingQueryData;

        // single-linked list used for invalidating chunk list caches
        public Archetype* NextChangedArchetype;

        public int EntityCount;
        public int ChunkCapacity;

        public int TypesCount;
        public int EnableableTypesCount;
        public int InstanceSize;
        public int InstanceSizeWithOverhead;
        public int ScalarEntityPatchCount;
        public int BufferEntityPatchCount;
        public ulong StableHash;

        // Index matches archetype types
        public int*    Offsets;
        public ushort* SizeOfs;
        public int*    BufferCapacities;

        // TypesCount indices into Types/Offsets/SizeOfs in the order that the
        // components are laid out in memory.
        public int* TypeMemoryOrder;

        // Order of components in the types array is always:
        // Entity, native component data, buffer components, managed component data, tag component, shared components, chunk components
        public short FirstBufferComponent;
        public short FirstManagedComponent;
        public short FirstTagComponent;
        public short FirstSharedComponent;
        public short FirstChunkComponent;

        public ArchetypeFlags Flags;

        public Archetype* CopyArchetype; // Removes system state components
        public Archetype* InstantiateArchetype; // Removes system state components & prefabs
        public Archetype* SystemStateResidueArchetype;
        public Archetype* MetaChunkArchetype;

        public EntityRemapUtility.EntityPatchInfo* ScalarEntityPatches;
        public EntityRemapUtility.BufferEntityPatchInfo* BufferEntityPatches;

        // @macton Temporarily store back reference to EntityComponentStore.
        // - In order to remove this we need to sever the connection to ManagedChangesTracker
        //   when structural changes occur.
        public EntityComponentStore* EntityComponentStore;

        public fixed byte QueryMaskArray[128];

        public bool SystemStateCleanupComplete => (Flags & ArchetypeFlags.SystemStateCleanupComplete) != 0;
        public bool SystemStateCleanupNeeded => (Flags & ArchetypeFlags.SystemStateCleanupNeeded) != 0;
        public bool Disabled => (Flags & ArchetypeFlags.Disabled) != 0;
        public bool Prefab => (Flags & ArchetypeFlags.Prefab) != 0;
        public bool HasChunkHeader => (Flags & ArchetypeFlags.HasChunkHeader) != 0;
        public bool HasBlobAssetRefs => (Flags & ArchetypeFlags.HasBlobAssetRefs) != 0;
        public bool HasManagedEntityRefs => (Flags & ArchetypeFlags.HasManagedEntityRefs) != 0;
        public bool HasCompanionComponents => (Flags & ArchetypeFlags.HasCompanionComponents) != 0;
        public bool HasWeakAssetRefs => (Flags & ArchetypeFlags.HasWeakAssetRefs) != 0;

        public int NumNativeComponentData => FirstBufferComponent - 1;
        public int NumBufferComponents => FirstManagedComponent - FirstBufferComponent;
        public int NumManagedComponents => FirstTagComponent - FirstManagedComponent;
        public int NumTagComponents => FirstSharedComponent - FirstTagComponent;
        public int NumSharedComponents => FirstChunkComponent - FirstSharedComponent;
        public int NumChunkComponents => TypesCount - FirstChunkComponent;
        public int NonZeroSizedTypesCount => FirstTagComponent;

        // These help when iterating specific component types
        // for(int iType=archetype->FirstBufferComponent; iType<archetype->BufferComponentsEnd;++iType) {...}
        public int NativeComponentsEnd => FirstBufferComponent;
        public int BufferComponentsEnd => FirstManagedComponent;
        public int ManagedComponentsEnd => FirstTagComponent;
        public int TagComponentsEnd => FirstSharedComponent;
        public int SharedComponentsEnd => FirstChunkComponent;
        public int ChunkComponentsEnd => TypesCount;

        public bool HasChunkComponents => FirstChunkComponent != TypesCount;

        public bool IsManaged(int typeIndexInArchetype) => Types[typeIndexInArchetype].IsManagedComponent;

        public override string ToString()
        {
            var info = "";
            for (var i = 0; i < TypesCount; i++)
            {
                var componentTypeInArchetype = Types[i];
                info += $"  - {componentTypeInArchetype}";
            }

            return info;
        }

        public void AddToChunkList(Chunk *chunk, SharedComponentValues sharedComponentIndices, uint changeVersion, ref EntityComponentStore.ChunkListChanges changes)
        {
            chunk->ListIndex = Chunks.Count;
            if (Chunks.Count == Chunks.Capacity)
            {
                var newCapacity = (Chunks.Capacity == 0) ? 1 : (Chunks.Capacity * 2);

                // The shared component indices we are inserting belong to the same archetype so they need to be adjusted after reallocation
                if (Chunks.InsideAllocation((ulong)sharedComponentIndices.firstIndex))
                {
                    int chunkIndex = (int)(sharedComponentIndices.firstIndex - Chunks.GetSharedComponentValueArrayForType(0));
                    Chunks.Grow(newCapacity);
                    sharedComponentIndices = Chunks.GetSharedComponentValues(chunkIndex);
                }
                else
                {
                    Chunks.Grow(newCapacity);
                }
            }

            Chunks.Add(chunk, sharedComponentIndices, changeVersion);

            fixed(Archetype* archetype = &this)
            {
                changes.TrackArchetype(archetype);
            }
        }

        public void RemoveFromChunkList(Chunk* chunk, ref EntityComponentStore.ChunkListChanges changes)
        {
            Chunks.RemoveAtSwapBack(chunk->ListIndex);
            var chunkThatMoved = Chunks[chunk->ListIndex];
            chunkThatMoved->ListIndex = chunk->ListIndex;

            fixed(Archetype* archetype = &this)
            {
                changes.TrackArchetype(archetype);
            }
        }

        void AddToChunkListWithEmptySlots(Chunk *chunk)
        {
            chunk->ListWithEmptySlotsIndex = ChunksWithEmptySlots.Length;
            ChunksWithEmptySlots.Add(chunk);
        }

        void RemoveFromChunkListWithEmptySlots(Chunk *chunk)
        {
            var index = chunk->ListWithEmptySlotsIndex;
            Assert.IsTrue(index >= 0 && index < ChunksWithEmptySlots.Length);
            Assert.IsTrue(ChunksWithEmptySlots.Ptr[index] == chunk);
            ChunksWithEmptySlots.RemoveAtSwapBack(index);

            if (chunk->ListWithEmptySlotsIndex < ChunksWithEmptySlots.Length)
            {
                var chunkThatMoved = ChunksWithEmptySlots.Ptr[chunk->ListWithEmptySlotsIndex];
                chunkThatMoved->ListWithEmptySlotsIndex = chunk->ListWithEmptySlotsIndex;
            }
        }

        /// <summary>
        /// Remove chunk from archetype tracking of chunks with available slots.
        /// - Does not check if chunk has space.
        /// - Does not check if chunk is locked.
        /// </summary>
        /// <param name="chunk"></param>
        internal void EmptySlotTrackingRemoveChunk(Chunk* chunk)
        {
            fixed (Archetype* archetype = &this)
            {
                Assert.AreEqual((ulong)archetype, (ulong)chunk->Archetype);
            }
            if (NumSharedComponents == 0)
                RemoveFromChunkListWithEmptySlots(chunk);
            else
                FreeChunksBySharedComponents.Remove(chunk);
        }

        /// <summary>
        /// Add chunk to archetype tracking of chunks with available slots.
        /// - Does not check if chunk has space.
        /// - Does not check if chunk is locked.
        /// </summary>
        /// <param name="chunk"></param>
        internal void EmptySlotTrackingAddChunk(Chunk* chunk)
        {
            fixed (Archetype* archetype = &this)
            {
                Assert.AreEqual((ulong)archetype, (ulong)chunk->Archetype);
            }
            if (NumSharedComponents == 0)
                AddToChunkListWithEmptySlots(chunk);
            else
                FreeChunksBySharedComponents.Add(chunk);
        }

        internal Chunk* GetExistingChunkWithEmptySlots(SharedComponentValues sharedComponentValues)
        {
            if (NumSharedComponents == 0)
            {
                if (ChunksWithEmptySlots.Length != 0)
                {
                    var chunk = ChunksWithEmptySlots.Ptr[0];
                    Assert.AreNotEqual(chunk->Count, chunk->Capacity);
                    return chunk;
                }
            }
            else
            {
                var chunk = FreeChunksBySharedComponents.TryGet(sharedComponentValues, NumSharedComponents);
                if (chunk != null)
                {
                    return chunk;
                }
            }

            return null;
        }

        internal bool CompareMask(EntityQueryMask mask)
        {
            return (byte)(QueryMaskArray[mask.Index] & mask.Mask) == mask.Mask;
        }

        internal void SetMask(EntityQueryMask mask)
        {
            QueryMaskArray[mask.Index] |= mask.Mask;
        }
    }
}
