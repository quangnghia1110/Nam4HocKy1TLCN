package studentConsulting.model.entity.authentication;

import java.sql.Timestamp;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;

@Data
@Builder
@Entity
@Table(name = "account")
@NoArgsConstructor
@AllArgsConstructor
public class AccountEntity{
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "department_id")
    @JsonBackReference
    private DepartmentEntity department;

    @OneToOne(fetch = FetchType.EAGER) 
    @JoinColumn(name = "role_id", nullable = false)
    private RoleEntity role;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "role_consultant_id")
    private RoleConsultantEntity roleConsultant;

    @Column(name = "email", nullable = false, length = 50)
    private String email;

    @Column(name = "is_activity", nullable = false)
    private boolean isActivity;

    @Column(name = "password", nullable = false, length = 255)
    private String password;

    @Column(name = "username", nullable = false, length = 50)
    private String username;

    @Column(name = "verify_code", length = 50)
    private String verifyCode;

    @Column(name = "verify_register", length = 50)
    private String verifyRegister;
    
    @Column(name = "verify_code_expiration_time")
    private Timestamp verifyCodeExpirationTime;

    @Column(name = "verify_code_attempt_count", nullable = false, columnDefinition = "int default 0")
    private int verifyCodeAttemptCount;

    
    @OneToMany(mappedBy = "account", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonManagedReference
    private Set<UserInformationEntity> users;
}
