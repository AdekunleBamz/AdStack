/**
 * React Hooks for Contract Interactions
 * Using Stacks.js v7+ with React Query
 */

'use client';

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { ClarityValue } from '@stacks/transactions';
import { callContract, TransactionOptions, TransactionResult } from '../lib/transaction-builder';
import { callReadOnly, ReadOnlyOptions, ReadOnlyResult } from '../lib/read-only-calls';
import { parseStacksError } from '../lib/error-handler';
import { useWalletStore } from '../store/wallet-store';

/**
 * Hook for contract write operations (mutations)
 */
export function useContractCall() {
  const queryClient = useQueryClient();

  return useMutation<TransactionResult, Error, TransactionOptions>({
    mutationFn: async (options) => {
      return await callContract(options);
    },
    onSuccess: (data, variables) => {
      // Invalidate related queries
      queryClient.invalidateQueries({
        queryKey: ['contract', variables.contractName],
      });
    },
    onError: (error) => {
      const parsedError = parseStacksError(error);
      console.error('Contract call failed:', parsedError);
    },
  });
}

/**
 * Hook for contract read-only operations
 */
export function useContractRead<T = any>(
  options: ReadOnlyOptions,
  enabled: boolean = true
) {
  return useQuery<ReadOnlyResult<T>, Error>({
    queryKey: ['contract', options.contractName, options.functionName, options.functionArgs],
    queryFn: () => callReadOnly<T>(options),
    enabled,
    staleTime: 30000, // 30 seconds
    refetchInterval: 60000, // Refetch every minute
  });
}

/**
 * Hook to get campaign details
 */
export function useCampaign(campaignId: number, enabled: boolean = true) {
  return useContractRead(
    {
      contractName: 'promo-manager',
      functionName: 'get-campaign-details',
      functionArgs: [{ type: 'uint', value: BigInt(campaignId) }],
    },
    enabled
  );
}

/**
 * Hook to get user profile
 */
export function useUserProfile(userAddress: string, enabled: boolean = true) {
  return useContractRead(
    {
      contractName: 'user-profiles',
      functionName: 'get-user-details',
      functionArgs: [
        {
          type: 'principal',
          value: { address: userAddress },
        },
      ],
    },
    enabled && !!userAddress
  );
}

/**
 * Hook to get analytics metrics
 */
export function useAnalytics(campaignId: number, enabled: boolean = true) {
  return useContractRead(
    {
      contractName: 'stats-tracker',
      functionName: 'get-campaign-metrics',
      functionArgs: [{ type: 'uint', value: BigInt(campaignId) }],
    },
    enabled
  );
}

/**
 * Hook to create a campaign
 */
export function useCreateCampaign() {
  const { mutate, ...rest } = useContractCall();

  const createCampaign = (params: {
    name: string;
    budget: bigint;
    dailyBudget: bigint;
    duration: number;
  }) => {
    mutate({
      contractName: 'promo-manager',
      functionName: 'create-campaign',
      functionArgs: [
        { type: 'string-ascii', value: params.name },
        { type: 'uint', value: params.budget },
        { type: 'uint', value: params.dailyBudget },
        { type: 'uint', value: BigInt(params.duration) },
      ],
    });
  };

  return { createCampaign, ...rest };
}

/**
 * Hook to register user
 */
export function useRegisterUser() {
  const { mutate, ...rest } = useContractCall();

  const registerUser = (params: {
    role: string;
    displayName: string;
  }) => {
    mutate({
      contractName: 'user-profiles',
      functionName: 'register-user',
      functionArgs: [
        { type: 'string-ascii', value: params.role },
        { type: 'string-ascii', value: params.displayName },
      ],
    });
  };

  return { registerUser, ...rest };
}

/**
 * Hook to check wallet connection
 */
export function useWallet() {
  const { address, isConnected } = useWalletStore();

  return {
    address,
    isConnected,
    isLoading: false,
  };
}

/**
 * Hook to get escrow balance
 */
export function useEscrowBalance(campaignId: number, enabled: boolean = true) {
  return useContractRead<bigint>(
    {
      contractName: 'funds-keeper',
      functionName: 'get-escrow-balance',
      functionArgs: [{ type: 'uint', value: BigInt(campaignId) }],
    },
    enabled
  );
}

/**
 * Hook to get publisher earnings
 */
export function usePublisherEarnings(
  publisherAddress: string,
  campaignId: number,
  enabled: boolean = true
) {
  return useContractRead<bigint>(
    {
      contractName: 'cash-distributor',
      functionName: 'get-publisher-earnings',
      functionArgs: [
        {
          type: 'principal',
          value: { address: publisherAddress },
        },
        { type: 'uint', value: BigInt(campaignId) },
      ],
    },
    enabled && !!publisherAddress
  );
}

/**
 * Hook to fund campaign
 */
export function useFundCampaign() {
  const { mutate, ...rest } = useContractCall();

  const fundCampaign = (params: { campaignId: number; amount: bigint }) => {
    mutate({
      contractName: 'promo-manager',
      functionName: 'fund-campaign',
      functionArgs: [
        { type: 'uint', value: BigInt(params.campaignId) },
        { type: 'uint', value: params.amount },
      ],
    });
  };

  return { fundCampaign, ...rest };
}

/**
 * Hook to pause campaign
 */
export function usePauseCampaign() {
  const { mutate, ...rest } = useContractCall();

  const pauseCampaign = (campaignId: number) => {
    mutate({
      contractName: 'promo-manager',
      functionName: 'pause-campaign',
      functionArgs: [{ type: 'uint', value: BigInt(campaignId) }],
    });
  };

  return { pauseCampaign, ...rest };
}

/**
 * Hook to resume campaign
 */
export function useResumeCampaign() {
  const { mutate, ...rest } = useContractCall();

  const resumeCampaign = (campaignId: number) => {
    mutate({
      contractName: 'promo-manager',
      functionName: 'resume-campaign',
      functionArgs: [{ type: 'uint', value: BigInt(campaignId) }],
    });
  };

  return { resumeCampaign, ...rest };
}
