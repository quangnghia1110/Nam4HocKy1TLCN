package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.address.ProvinceEntity;

public interface ProvinceRepository extends  JpaRepository<ProvinceEntity, String>{

}
