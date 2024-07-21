package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.address.DistrictEntity;


public interface DistrictRepository extends  JpaRepository<DistrictEntity, String>{

}
