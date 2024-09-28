package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;

public interface RoleConsultantRepository extends JpaRepository<RoleConsultantEntity, Integer> {

}
