package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.user.RoleConsultantEntity;

public interface RoleConsultantRepository extends JpaRepository<RoleConsultantEntity, Integer> {

}
