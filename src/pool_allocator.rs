use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::UnsafeCell;
use std::ops::Deref;

const BUCKET_SIZE: usize = 128;

struct PoolAllocator<T> {
  buckets: UnsafeCell<Vec<*mut u8>>,
  freelist: UnsafeCell<Vec<*mut T>>,
}

struct Droplet<'pool, T> {
  obj: *mut T,
  source: &'pool PoolAllocator<T>,
}

impl<'pool, T> Deref for Droplet<'pool, T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    unsafe { &*self.obj }
  }
}

impl<'pool, T> Drop for Droplet<'pool, T> {
  fn drop(&mut self) {
    unsafe {
      std::ptr::drop_in_place(self.obj);
    }
    self.source.freelist_mut().push(self.obj);
  }
}

impl<T> PoolAllocator<T> {
  pub fn new() -> Self {
    PoolAllocator {
      buckets: UnsafeCell::new(Vec::new()),
      freelist: UnsafeCell::new(Vec::new()),
    }
  }

  fn freelist_mut(&self) -> &mut Vec<*mut T> {
    unsafe { &mut *self.freelist.get() }
  }

  fn buckets_mut(&self) -> &mut Vec<*mut u8> {
    unsafe { &mut *self.buckets.get() }
  }

  pub fn alloc(&self, x: T) -> Droplet<T> {
    if self.freelist_mut().is_empty() {
      let b = unsafe {
        System.alloc(
          Layout::from_size_align(
            std::mem::size_of::<T>() * BUCKET_SIZE,
            std::mem::align_of::<T>(),
          )
          .unwrap(),
        )
      };

      self.buckets_mut().push(b);

      for i in 0..BUCKET_SIZE {
        self.freelist_mut().push(unsafe { (b as *mut T).add(i) });
      }
    }

    let p = self.freelist_mut().pop().unwrap();
    unsafe {
      std::ptr::write(p, x);
    }

    Droplet {
      obj: p,
      source: &self,
    }
  }
}

impl<T> Drop for PoolAllocator<T> {
  fn drop(&mut self) {
    assert_eq!(
      self.freelist_mut().len(),
      self.buckets_mut().len() * BUCKET_SIZE
    );

    for p in self.buckets_mut() {
      unsafe {
        System.dealloc(
          *p,
          Layout::from_size_align(
            std::mem::size_of::<T>() * BUCKET_SIZE,
            std::mem::align_of::<T>(),
          )
          .unwrap(),
        )
      };
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn basic_usage_test() {
    let pool = PoolAllocator::<usize>::new();
    let a: Droplet<usize>;
    let d = pool.alloc(12);
    a = pool.alloc(123);

    assert_eq!(*d, 12);
    assert_eq!(*d + *a, 12 + 123);
  }
}
