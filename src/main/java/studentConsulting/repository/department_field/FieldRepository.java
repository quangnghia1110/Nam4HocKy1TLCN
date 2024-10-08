package studentConsulting.repository.department_field;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.department_field.FieldEntity;

import java.util.Optional;

public interface FieldRepository extends PagingAndSortingRepository<FieldEntity, Integer>, JpaSpecificationExecutor<FieldEntity>, JpaRepository<FieldEntity, Integer> {
    Optional<FieldEntity> findByName(String name);
}
