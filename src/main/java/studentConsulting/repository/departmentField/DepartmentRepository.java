package studentConsulting.repository.departmentField;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;

public interface DepartmentRepository extends  JpaRepository<DepartmentEntity, Integer>{
	DepartmentEntity findByName(String name);

}
