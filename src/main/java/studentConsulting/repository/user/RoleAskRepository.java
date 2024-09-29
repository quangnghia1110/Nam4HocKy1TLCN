package studentConsulting.repository.user;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.user.RoleAskEntity;

public interface RoleAskRepository extends JpaRepository<RoleAskEntity, Integer> {

}
