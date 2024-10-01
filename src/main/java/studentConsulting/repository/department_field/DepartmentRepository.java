package studentConsulting.repository.department_field;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import studentConsulting.model.entity.department_field.DepartmentEntity;

import java.util.List;

public interface DepartmentRepository extends JpaRepository<DepartmentEntity, Integer> {
    @Query("SELECT d.id FROM DepartmentEntity d JOIN d.accounts a WHERE a.id = :advisorId")
    List<Integer> findDepartmentsByManagerId(@Param("advisorId") Integer advisorId);

}
