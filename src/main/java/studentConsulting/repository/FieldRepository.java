package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.departmentField.FieldEntity;

public interface FieldRepository extends JpaRepository<FieldEntity, Integer>{

}
