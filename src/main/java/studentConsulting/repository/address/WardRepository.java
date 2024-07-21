package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.address.WardEntity;

public interface WardRepository extends  JpaRepository<WardEntity, String>{

}
