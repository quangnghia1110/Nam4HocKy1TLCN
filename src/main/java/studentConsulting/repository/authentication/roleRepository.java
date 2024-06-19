package studentConsulting.repository.authentication;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import studentConsulting.entity.authentication.roleEntity;

@Repository
public interface roleRepository extends JpaRepository<roleEntity, Long> {

    roleEntity findByName(String name);
}
