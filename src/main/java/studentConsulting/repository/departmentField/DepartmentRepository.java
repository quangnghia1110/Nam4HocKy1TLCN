package studentConsulting.repository.departmentField;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.departmentField.DepartmentEntity;

public interface DepartmentRepository extends  JpaRepository<DepartmentEntity, Integer>{

}
