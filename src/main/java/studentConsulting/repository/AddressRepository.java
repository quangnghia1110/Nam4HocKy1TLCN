package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.address.AddressEntity;


public interface AddressRepository extends  JpaRepository<AddressEntity, Integer>{

}
