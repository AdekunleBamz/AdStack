'use client';

import { ThemeProvider } from '@/contexts/ThemeProvider';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useState } from 'react';
import { AppKitProvider } from '@/contexts/AppKitProvider';

export function Providers({ children }: { children: React.ReactNode }) {
  const [queryClient] = useState(() => new QueryClient({
    defaultOptions: {
      queries: {
        staleTime: 60 * 1000,
        refetchOnWindowFocus: false,
      },
    },
  }));

  return (
    <ThemeProvider>
      <QueryClientProvider client={queryClient}>
        <AppKitProvider>
          {children}
        </AppKitProvider>
      </QueryClientProvider>
    </ThemeProvider>
  );
}
