using Unity.Entities;

[assembly: DisableAutoCreation]

namespace Unity.Entities.Tests.InAnotherAssembly
{
    public struct MySentinelGenericComponent_FromAnotherAssembly<T> : IComponentData
    {
    }
    public partial struct MyGenericISystem_FromAnotherAssembly<T> : ISystem
    {

        public void OnCreate(ref SystemState state)
        {
            state.EntityManager.CreateEntity(ComponentType.ReadWrite<MySentinelGenericComponent_FromAnotherAssembly<T>>());
        }
    }

    public static class MyGenericNestingType<T>
    {
        public partial struct MyNestedGenericISystem : ISystem
        {
            public void OnCreate(ref SystemState state)
            {
                state.EntityManager.CreateEntity(ComponentType.ReadWrite<MySentinelGenericComponent_FromAnotherAssembly<MyNestedGenericISystem>>());
            }
        }
            
    }

    public struct EcsTestDataInAnotherAssembly : IComponentData
    {
        public int value;
    }

    public struct EcsTestData2InAnotherAssembly : IComponentData
    {
        public int value0;
    }

    public partial struct SimplestCaseJobInAnotherAssembly : IJobEntity
    {
        void Execute(ref EcsTestDataInAnotherAssembly e1, in EcsTestData2InAnotherAssembly e2) => e1.value += e2.value0;
    }
}
