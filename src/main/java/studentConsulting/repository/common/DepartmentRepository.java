package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.department_field.DepartmentEntity;

public interface DepartmentRepository extends JpaRepository<DepartmentEntity, Integer> {

}
