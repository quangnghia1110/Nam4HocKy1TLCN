package studentConsulting.repository.department_field;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.department_field.FieldEntity;

public interface FieldRepository extends PagingAndSortingRepository<FieldEntity, Integer>, JpaSpecificationExecutor<FieldEntity>, JpaRepository<FieldEntity, Integer> {

}
