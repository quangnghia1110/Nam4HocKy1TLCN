package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.department_field.FieldEntity;

public interface FieldRepository extends JpaRepository<FieldEntity, Integer> {

}
